(*
  cfg_prod.ml --- Functions creating the CFG from a FG and a specification DFA
*)


open Cfg
open Dfa
open Flowgraph



let rec call_edges_to_prod (d : dfa) (fg :flowgraph) (qa : state) (qd : state) (vj : node) (methods : meth list) (res : sentential_form list ref) : unit = 
  match methods with 
  | [] -> ()
  | hd :: tl when String.equal hd "eps" -> call_edges_to_prod d fg qa qd vj tl res
  | hd :: tl when not (List.mem hd d.alphabet) -> call_edges_to_prod d fg qa qd vj tl res
  | hd :: tl -> 
        let vk = get_entry_node fg hd in 

        for k=0 to (d.nb_states - 1) do 
          for l=0 to (d.nb_states - 1) do
            res := [Var(Tuple(qa, Terminal(hd), k)); Var(Tuple(k, Node(vk), l)); Var(Tuple(l, Node(vj), qd))] :: !res
          done
        done;

        call_edges_to_prod d fg qa qd vj tl res;;



let prod_cfg (d : dfa) (fg : flowgraph) (v : variable) : sentential_form list = 
  match v with
  | Start -> 
        let v0 = get_entry_node fg "main" in 
        let q0 = d.initial in 
        List.map (fun x -> [Var(Tuple(q0, Node(v0), x))]) (d.final)
  | Tuple(qa, Node(vi), qb) ->
        let res = ref [] in 
        let n = List.length fg.nodes in 

        for vj=0 to (n-1) do 
          if List.exists (String.equal "eps") fg.edge_func.(vi.id).(vj) then 
            (res := [Var(Tuple(qa, Node(List.nth fg.nodes vj), qb))] :: !res)
        done;

        for vj=0 to (n-1) do 
          call_edges_to_prod d fg qa qb (List.nth fg.nodes vj) fg.edge_func.(vi.id).(vj) res
        done;

        if vi.return then 
          (res := [Terminal("eps")] :: !res);

        !res

  | Tuple(qa, Terminal(met), qb) ->
        if (List.exists (String.equal met) d.delta.(qa).(qb)) then 
          [[Terminal(met)]]
        else 
          [];;



let var_cfg (d : dfa) (fg : flowgraph) : variable list = 
  let res = ref [Start] in

  for i=0 to (d.nb_states - 1) do
    for j=0 to (d.nb_states - 1) do 
      for k=0 to (List.length fg.nodes - 1) do 
        res := Tuple(i, Node(List.nth fg.nodes k), j) :: !res
      done;

      for k=0 to (List.length d.alphabet - 1) do 
        res := Tuple(i, Terminal(List.nth d.alphabet k), j) :: !res
      done
    done
  done;

  !res;;

let cfg_prod (d : dfa) (fg : flowgraph) : cfg = 
  {
    variables = var_cfg d fg;
    terminals = d.alphabet;
    productions = prod_cfg d fg
  };;



