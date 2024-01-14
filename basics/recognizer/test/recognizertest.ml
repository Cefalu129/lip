open Recognizer


let%test _ = belongsTo ['0';'0';'1';'0'] = [true; false; true; false; false]
let%test _ = belongsTo ['0'; '0'; '1'; '1'; '0'; '0'; '1'; '1'] = [true; false; false; false; true]
let%test _ = belongsTo ['1'; '1'; '0'; '0'; '1'; '1'; '0'] = [true; false; false; false; false]
let%test _ = belongsTo ['1'; '0'; '1'] = [true; false; false; true; false]
let%test _ = belongsTo ['1'; '0'; '1'; '0'; '1'; '0'; '1'; '0'; '1'] = [true; false; false; false; false]
let%test _ = belongsTo ['1'; '1'; '0'; '0'] = [true; false; false; true; true]
let%test _ = belongsTo ['1'; '0'; '1'; '0'; '2'; '0'; '1'] = [false; false; false; false; false]
let%test _ = belongsTo ['1'; '0'; '0'; '1'; '0'] = [true; false; false; true; false]
let%test _ = belongsTo ['0'; '1'; '1'; '0'; '1'; '0'] = [true; false; true; false; false]
let%test _ = belongsTo ['0'] = [true; true; false; false; false]
let%test _ = belongsTo ['0'; '1'] = [true; true; false; false; false]
let%test _ = belongsTo ['0'; '1'; '4'; '0'; '1'] = [false; false; false; false; false]
let%test _ = belongsTo ['0'; '1'; '1'; '1'; '1'] = [true; true; false; false; false]