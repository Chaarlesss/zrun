# The ZRun Abstract Interpreter

## Description générale

Ce projet a été réalise dans le cadre du cours "[Systèmes réactifs synchrones](https://wikimpri.dptinfo.ens-cachan.fr/doku.php?id=cours:c-2-23-1)" du MPRI.

Celui-ci consiste en la création d'un analyseur statique par [interprétation abstraite](https://www.di.ens.fr/~cousot/COUSOTpapers/publications.www/CousotCousot-POPL-77-ACM-p238--252-1977.pdf) pour le langage Zelus en se basant sur un interpréteur co-itératif du langage.
La particularité d'un interpréteur abstrait est de pouvoir déterminer des propriétés sur des programmes, comme l'absence d'erreurs arithmétiques, de dépassements d'entiers ou la régularité de certaines fonctions.
Dans notre projet et à long terme, il était espéré de pouvoir faire des statiques de programmes Zelus afin de pouvoir détecter certains invariants arithmétiques (comme le fait qu'une variable soit bornée) et prouver l'absence d'erreurs arithmétiques à l'exécution (comme la division par zéro).

## Méthodologie adoptée

La principale difficulté du projet vient du fait que les langages synchrones définissent une sémantique non-usuelle (comparée à des langages de programmation comme C, Python ou OCaml), dont l'analyse statique a été relativement peu décrite dans la littérature.
Afin de déduire facilement quelle sémantique abstraite donner à notre interpréteur (sans perdre du temps à la calculer pour prouver sa sûreté), nous avons décidé de faire une analogie avec le code C qui aurait pu être produit par un compilateur Zelus, en basant par la sémantique qui est implicitement décrite par l'interpréteur original.

L'abstraction de l'interpréteur également présente une autre difficulté: certaines données sont cachés à l'exécution dans l'état d'un noeud, qui contient notamment des informations sur les valeurs (souvent entières) précédentes de certaines variables.
Plusieurs choix se sont alors offerts à nous:
 - utiliser une abstraction cartésienne (aussi appelée une abstraction non relationelle) sur ces données afin de conserver le même squelette de structure pour ces données, au prix d'une perte de précision
 - remplacer toutes ces valeurs par des variables fantômes, dont des informations seront contenues dans le domaine abstrait qui est un général à chaque point d'exécution du programme. Avec une telle solution, il est théoriquement possible d'implémenter un interpréteur abstrait qui calcule exactement tous les traces d'exécution (même si celui-ci pourrait ne pas terminer à cause du théorème de Rice)
 - définir un (ou des) abstraction(s) spécifique(s) pour représenter l'état d'un noeud
 
 La solution que nous avons essayé d'adopter a été la seconde solution, avec comme espoir de pouvoir se reposer sur les domaines abstraits définis dans la bibliothéque [APRON](https://antoinemine.github.io/Apron/doc/) pour vérifier facilement des invariants numériques complexes. 

## Travail réalisé

Notre travail réalisé peut se découper en deux parties distinctes dans l'interpréteur abstrait.
À titre informatif, nous tenons à souligner que pour des raisons d'organisation, nous avons décidé de séparer l'interpréteur original en plusieurs modules afin de distinguer facilement ce qui concerne uniquement la représentation syntaxique d'un programme Zelus de son exécution.
Ainsi, nous avons fait nos (principaux) ajouts dans le module `analyzer`, qui dépend du module `base` contenant notamment l'arbre de syntaxe abstraite.

D'une part, nous avons implémenté certains domaines cartésiens à savoir le domaine des intervalles (qui représente les ensembles convexes de l'ensemble des entiers), le domaine des signes (qui représente uniquement le signe d'une variable) et le domaine de parité.
Tous ces domaines possèdent une signature commune qui aurait pu permettre de paramétrer facilement l'analyseur par un domaine abstrait différent en fonction de ce que nous souhaitons vérifier.
Un produit réduit entre ces domaines ainsi que le domaine des congruences étaient également en cours d'implémentation.

D'autre part, nous avons fait une écriture partielle de l'interpréteur abstrait, notamment en définissant la structure de données qui est utilisée pour abstraire un noeud, ainsi qu'en écrivant la sémantique de certaines commandes à l'initialisation.

Décrivons en particulier la structure qui représente un noeud:

```OCaml
type ('a, 'b, 'c, 's, 'e) anode =
  (** ... *)
  | ACoNode : {
      init: ('a -> (('b * 's), 'e) Result.t);
      step: ('s -> 'a -> 'c -> ('b, 'e) Result.t)
    } -> ('a, 'b, 'c, 's, 'e) anode
```

Toutes les opérations de l'interpréteur abstrait retournent une monade qui permet de contenir les informations sur les éventuelles erreurs détectées par l'analyseur, qui sont de type `'e`.

Concernant le champ `init`, celui-ci contient la fonction qui sera exécutée lors de l'étape d'initialisation du noeud.
Il prend en paramètre un élément du domaine abstrait (ici de type `'a`), qui est l'environnement abstrait avant l'initialisation du noeud, et renvoie un état abstrait (de type `'s`), où toutes les valeurs ont été remplacées par des variables fantômes, et un autre élément du domaine abstrait (ici de type `'b`), qui est l'environnement abstrait après l'initiation du noeud et qui peut donc contenir des informations sur les variables fantômes contenues dans l'état abstrait.

Concernant le champ `step`, celui-ci contient la fonction qui sera executée à chaque boucle d'horloge.
Les types `'s`, `'a`, `'b` et `'e` représentent toujours des entités qui remplissent des fonctions similaires à celle de l'étape d'initialisation.
Il est important de noter que l'état abstrait (qui est de type `'s`) est ici passé en argument car celui-ci n'est pas voué à changer lors de l'exécution, mais il est supposé être distinct pour chaque instance du noeud.
Enfin, le paramètre de type `'c` représente les éventuels arguments que nous pourrions donner à un noeud. Ici, le polymorphisme du type laisse un certains choix à l'implémentation:
 - soit le type `'c` contient des variables dont les informations sont déjà contenues dans l'élement abstrait (qui est de type `'a`)
 - soit le type `'c` contient des expressions symboliques, dont le contenu ne pourrait être utilisé que si cela est nécessaire par l'interpréteur abstrait
La deuxième solution, qui est celle que nous avons choisi d'implémenter, vient originellement du fait que traditionnellement, les domaines abstraits calculent le résultat d'un assignement de variable en prenant en entrée une expression symbolique afin de pouvoir réaliser un rafinement du résultat.
En particulier, la librairie APRON que nous prévoyions d'utiliser fait usage de ce mécanisme.

Bien entendu, une mécanisme d'élargissement (*widening*) est utilisé à chaque boucle d'exécution afin de garantir la terminaison de l'analyseur statique et obtenir un résultat qui sur-approxime le comportement du programme analysé.

Cependant, nous avons vite confronté aux problèmes de causalité et de non-initialisation des variables qui, pour être résolus, demandaient à effectuer des modifications dans les représentations des éléments du domaine abstrait que nous n'avions pas anticipé.
De plus, nous avions initialement voulu conserver les types entier et booléen dans notre interpréteur abstrait, mais l'abstraction des variables nécessitait de connaître leur type afin de déterminer quel domaine utiliser pour les représenter.
Cette option nécessitait donc l'implémentation d'un système de type, ce qui n'était pas le but du projet.
Enfin, l'utilisation d'expressions symboliques dans les opérations des domaines abstraits nécessitait d'utiliser des expressions simplifiées ne contenant que des opérations arithmétiques.
Il aurait donc été nécessaire de trouver un moyen de transformer l'AST en AST simplifiée où les expressions ne contiendraient pas de `if then else`, de `fby` ou de déclaration de variables, ce qui ne nous semblait pas être une tâche simple à réaliser.
Bien évidemment, réaliser des simplifications afin de contourner ces problèmes résulteraient probablement en un langage trop peu expressif pour profiter des fonctionnalités offertes pour un langage réactif.

## Conclusion

Nous avons donc essayé d'implémenter un analyseur statique par interprétation abstraite pour le langage de programmation réactif Zélus, mais nous n'avons malheureusement pas réussi à mener notre projet à terme.

En effet, même si nous jugeons que le sujet en lui-même est très intéressant, il est certain que nous l'avons abordé sous un mauvais angle: une étude théorique du problème au préalable aurait sans doute pu être judicieuse.
De plus, la représentation des états d'un noeud dans le domaine abstrait constitue véritablement tout le coeur du problème (par rapport à un langage impératif classique), et nous n'en avons donné qu'une solution très naïve en choisissant de représenter tous les champs d'un état par des variables fantômes.
Compte tenu du cadre de l'interprétation abstraite dans lequel nous nous sommes situées, il serait certainement préférable de développer des domaines abstraits particuliers pour représenter l'état d'un noeud.
Cette idée peut en partie être aperçue dans des domaines abstraits numériques spécifiques à l'analyse de la traduction C de programmes réactifs, comme le [domaine arithmético-géométrique](https://www.di.ens.fr/~feret/publications/vmcai2005.pdf) ou le [domaine des filtres](https://www.di.ens.fr/~feret/publications/esop2004.pdf), et devrait pouvoir trouver son plein potentiel avec une analyse ayant plus d'informations sur la structure initiale du programme.

Enfin, l'implémentation actuelle de l'analyseur en l'état n'est qu'un brouillon de ce qu'il pourrait réellement être.
Cela s'explique notamment par la volonté d'avoir une implémentation similaire à celle de l'interpréteur original, alors que les deux interpréteurs ont deux fonctionnements qui sont différents sur bien des points.
Avec du recul, il existe plusieurs solutions pour obtenir un analyseur plus facile à implémenter, vis-à-vis des difficultés que nous avons recontré.
Une première solution serait de s'inspirer des transformations de programmes synchrones en des programmes impératifs réalisé par le compilateur vérifié [Velus](https://velus.inria.fr) afin de se ramener dans un cadre classique de l'interprétation abstraite, où toutes les expressions seraient "pures".
Une seconde solution pourrait être d'implémenter une forme dynamique d'interprétation abstraite tel que cela est réalisé dans l'analyseur statique [MOPSA](http://mopsa.lip6.fr), permettant de réaliser peu d'efforts au niveau de la transformation d'un programme Zelus et changer la sémantique de certaines constructions, comme `fby` ou `pre` en fonction de l'élément du domaine abstrait.
Dans tous les cas, ces solutions demandent implicitement de fournir un système de type pour Zelus, mais nous avons découvert par la suite qu'un étant déjà à disposition dans [Zelus](https://github.com/INRIA/zelus).

Ainsi, ce projet présente encore beaucoup de pistes d'amélioration, qui viennent parfois avec des solutions, ce qui nous laisse espérer qu'il serait possible d'obtenir des résultats satisfaisants dans le futur. 

# The ZRun Interpreter

ZRun is an interpreter for a synchronous data-flow language. The input
of Zrun is a first-order subset of Zelus (with the same syntax) and is
only discrete-time. Programs can mix data-flow equations and
hierarchical automata as it exists in Scade. States in automata can be
parameterized (this feature is not provided by Scade; it is described
in the paper [EMSOFT'06] by Colaco et al.). The long term goal is to
treat all Zelus programs (we are far away from that!).  Constructs to
deal with continuous-time (ODE and zero-crossing events), higher-order
functions, arrays are not considered for the moment.

One objective is to give a reference and executable
semantics for a language like Scade that can be used: to test
an existing compiler; to prove compilation steps (e.g.,
that a well typed/causal/initialized program does not lead to an
error; or to prove semantics preservation of source-to-source
transformations like static scheduling or the compilation of
automata); to execute unfinished programs or programs that are
semantically correct but are statically rejected by the compiler.
Examples are cyclic circuits accepted by an Esterel compiler (the
so-called "constructively causal" programs) but are rejected by
Lustre, Lucid Synchrone, Scade, Zelus compilers that impose stronger
causality constraints; to prototype new language constructs.


Zrun defines an executable denotational semantics. It builds on two
papers which defines the semantics of a data-flow language through the
computation of a fix-point at every reaction step: 1/ "A Coiterative
Characterization of Synchronous Stream Functions", by Caspi and
Pouzet, CMCS, 1998 (VERIMAG tech. report, 1997); 2/ "The semantics and
execution of a synchronous block-diagram language", by Edwards and
Lee, Science of Computer Programming 2006.

If you find this work useful or have any
comment/question/criticism, please send a mail to Marc.Pouzet@ens.fr.

## Getting Started

The interpreter is written in OCaml mostly in purely functional style.
The simplest way to install the dependencies is via [OPAM](https://opam.ocaml.org/).

```bash
opam install dune menhir
```

Then to build the interpreter:

```bash
make
```

This will generate a `zrun.exe` executable.

```bash
 ./zrun.exe --help
Options are:
  -s         The main node to evaluate
  -n         The number of steps
  -check     Check that the simulated node returns true
  -v         Verbose mode
  -noassert  No check of assertions
  -help      Display this list of options
  --help     Display this list of options
```

## Examples

Examples are located in the `tests` directory.
Consider for instance the simple chronometer in `tests/chrono_in_scade.zls` 
(we use small constants in the counters to speedup the outputs).

```
(*
file watch_in_scade.zls
This example is adapted from a classical example from Scade

-------------------------- Watch Interface-------------------------
-- stst : start/stop button
-- rst : reset button
-- set : set time button
-- md : mode selection button
-- a1, a2, a3 : time data display
-- l_ : is displaying lap time
-- s_ : is in setting time mode
-- sh_ : is in setting hour mode
-- s_ and not sh_ : is in setting minutes mode
-------------------------------------------------------------------
 *)

let node root (stst,rst,set,md) returns (a1, a2, a3, l_, s_, sh_ )
local
  isStart default false, (* -- is the chrono started? *)
  is_w default false, (* -- is watch in clock mode? *)
  sigS default false,
  sigSh default false,
  sigL default false,
  m init 0, s init 0, d init 0, (* -- chrono timers *)
  last wh, last wm, last ws, last w (* -- clock timers *)
do
  l_ =  sigL
and
  s_ =  sigS
and
  sh_ =  sigSh
and
  automaton (* -- Chrono ----------------------*)
  | Stop ->
      do
	m, s, d = (0, 0, 0) -> (last m, last s, last d)
      unless
        (stst && not is_w) continue Start
      else (rst && not (false -> pre l_) && not is_w) then Stop
  | Start ->
      do
        d = (last d + 1) mod 100
      and
	s = (if (d < last d) then last s + 1 else last s) mod 60
      and
	m = if (s < last s) then last m + 1 else last m
      and
	isStart = true
      unless (stst && not is_w) continue Stop
  end
and
  automaton (* -- Watch ------------------*)
  | Count ->
      do
        wm = 0 -> (if (ws < last ws)
	           then last wm + 1 else last wm) mod 60
      and
	wh = 0 -> (if (wm < last wm)
	           then last wh + 1 else last wh) mod 24
      until (set && is_w) then Set
  | Set -> (* -- Set time *)
      local synchro default false
      do
        sigS = true
      and
        automaton (* -- set Watch -----------*)
        | Set_hr -> (* -- set hour first *)
            do
              sigSh = true
	    and
              wh = (if stst then last wh + 1
                    else if rst then last wh +23
                    else last wh) mod 24
	    until set then Set_mn
        | Set_mn -> (* -- then set minutes *)
            do
              wm = (if stst then last wm + 1
                    else if rst then last wm +59
                    else last wm) mod 60
	    until set then Set_end
        | Set_end -> do synchro = true done
	end
      until synchro continue Count
  end
and
    w = 0 -> (pre w + 1) mod 100
and
    ws = 0 -> (if (w < pre w) then pre ws + 1 else pre ws) mod 60
and  
  automaton (* -- Display ----------------*)
  | DispClk -> (* -- display watch *)
    do
      is_w = true
    and
      a1, a2, a3 = (wh, wm, ws)
    unless (md && not s_) continue DispChr
  | DispChr ->(* -- display chrono *)
    local
	lm init 0, ls init 0, ld init 0
	(* -- chrono display (to deal with lap time) *)
    do
        a1, a2, a3 = (lm, ls, ld)
    and
	automaton (* -- deal with lap time and current time ---*)
	| DispTime ->
            do
              lm, ls, ld = (m, s, d)
            unless (rst && isStart) then DispLap
	| DispLap ->
            do
              sigL = true
	    unless (rst) then DispTime
        end
    unless md continue DispClk
  end
done

let node counter(n) returns (ok)
  local c
  do
      c = 0 -> (pre c + 1) mod n
  and
      ok = (c = 0)
  done
      
let node main () returns (a1, a2, a3, l, s, sh)
  local stst, rst, set, md
  do
      stst = counter(5)
  and
      rst = counter(10)
  and
      set = counter(20)
  and
      md = counter(30)
  and
      (a1, a2, a3, l, s, sh) = root (stst, rst, set, md)
  done
      
```

The file `tests/watch_in_scade.zls` also contains a `main` node to simulate one possible execution.
To run this example for 30 steps:

```bash
./zrun.exe -s main -n 30 tests/watch_in_scade.zls
```

The following is a classical example of a cyclic program that is
statically rejected by the Lustre/Scade/Lucid Synchrone/Zelus
compilers while it is a valid Esterel program. This example is due to
Robert de Simone and is described by Gerard Berry in the Esterel primer
V5.91 of 2000. It is also used as an example to illustrate the
fixpoint semantics presented in the paper: "The semantics and
execution of a synchronous block-diagram language", Stephen Edwards
and Edward Lee, SCP, 2003.

```
(* file arbiter.zls *)

(* the two boolean operators are sequential, not symetric as *)
(* in Esterel and SCP paper. In the current semantics all imported *)
(* functions are strict, hence preventing *)
(* to have or(true, _) = or(_, true) = true with _ possibly bot *)
let node sequential_and_gate(x,y) returns (z)
    if x then z = y else z = false

let node sequential_or_gate(x,y) returns (z)
    if x then z = true else z = y

let node and_gate(x,y) returns (z)
    z = x && y

let node strict_or_gate(x,y) returns (z)
    z = x or y

let node arbiter(i, request, pass_in, token_in) returns (grant, pass_out, token_out)
  local o
  do
    grant = and_gate(request, o)
  and
    pass_out = and_gate(not request, o)
  and
    o = or_gate(token_in, pass_in)
  and
    token_out = i fby token_in
  done
      
let node arbiter_three(i, request1, request2, request3) returns (grant1, grant2, grant3)
  local pass_out1,
        pass_out2,
        pass_out3,
        token_out1,
        token_out2,
        token_out3
  (* the following set of equations is cyclic if we build an
  unconditional dependence graph *)
  do
    grant1, pass_out1, token_out1 = arbiter(request1, pass_out3, token_out3)
  and
    grant2, pass_out2, token_out2 = arbiter(request2, pass_out1, token_out1)
  and
    grant3, pass_out3, token_out3 = arbiter(request3, pass_out2, token_out2)
  done

let node main() returns (grant1, grant2, grant3) 
  local request1, request2, request3
  do
    request1 = true
  and
    request2 = true
  and
    request3 = true
  and
    grant1, grant2, grant3 = arbiter_three(request1, request2, request3)
  done
```

See other examples in directory tests/


