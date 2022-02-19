type t = {
  startpos: Lexing.position;
  endpos: Lexing.position
}

let make startpos endpos = {
  startpos = startpos;
  endpos = endpos
}

let startpos { startpos; _ } = startpos

let endpos { endpos; _} = endpos
