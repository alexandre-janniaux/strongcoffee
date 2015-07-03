open Sys

let istr = string_of_int



type benchmark = 
  { 
    name: string;
    start: float;
    stop: float;
    context: string;
    infos: string
  }

let current_context = ref "global"

let benchmark_list = ref []

let start_record name infos = {
  name = name;
  start = Sys.time ();
  stop = Sys.time ();
  context = !current_context;
  infos = infos
}

let stop_record record = 
  let record = {
    name=record.name;
    start=record.start;
    stop=Sys.time();
    context=record.context;
    infos=record.infos
  } in
  benchmark_list := record::!benchmark_list 

let dump_record name = 
  let l = List.filter (fun r -> r.name = name) !benchmark_list in
  print_string ("[BENCHMARK]: "^name^"\n");
  List.iter (fun r ->
              let duration = r.stop -. r.start in
              print_string ("+ Contexte : " ^ r.context ^ "\t Durée : " ^ string_of_float duration ^ " ms \n");
              print_string ("\tInfos : " ^ r.infos);
              print_newline();
            ) l



let (=@) a b = 
  stop_record b; a


