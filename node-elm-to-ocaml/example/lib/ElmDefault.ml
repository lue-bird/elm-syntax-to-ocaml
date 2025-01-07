let basics_eq : 'a -> 'a -> bool =
    fun a b -> a = b
let basics_neq : 'a -> 'a -> bool =
    fun a b -> a <> b

let basics_lt : 'a -> 'a -> bool =
    fun a b -> a < b
let basics_gt : 'a -> 'a -> bool =
    fun a b -> a > b
let basics_le : 'a -> 'a -> bool =
    fun a b -> a <= b
let basics_ge : 'a -> 'a -> bool =
    fun a b -> a >= b
type basics_Order = | Basics_LT | Basics_EQ | Basics_GT
let intToBasics_order: int -> basics_Order =
    fun comparisonInt ->
        if comparisonInt < 0 then
            Basics_LT
        else if comparisonInt > 0 then
            Basics_GT 
        else
            Basics_EQ
let basics_orderToInt: basics_Order -> int =
    fun order ->
        match order with
        | Basics_LT -> -1
        | Basics_EQ -> 0
        | Basics_GT -> 1
let basics_compare : 'a -> 'a -> basics_Order =
    fun a b ->
        intToBasics_order (Stdlib.compare a b)

let basics_modBy =
    fun divisor toDivide ->
        let remainder = Float.div toDivide divisor
        in
        if
            (remainder > 0. && divisor < 0.)
                || (remainder < 0. && divisor > 0.)
        then
            remainder +. toDivide
        else
            remainder
let basics_idiv : float -> float -> float =
    fun a b -> Float.trunc (Float.div a b)

let basics_or : bool -> bool -> bool =
    fun a b -> Bool.(||) a b
let basics_and : bool -> bool -> bool =
    fun a b -> Bool.(&&) a b

let char_toCode : char -> float =
    fun char -> Int.to_float (Char.code char)
let char_fromCode : float -> char =
    fun char -> Char.chr (Float.to_int char)

let string_toInt : string -> float option =
    fun string -> Option.map Float.of_int (Stdlib.int_of_string_opt string)

let list_singleton : 'a -> 'a list =
    fun onlyElement -> [ onlyElement ]
let rec list_drop : float -> 'a list -> 'a list =
    fun skippedCount list ->
        if skippedCount <= 0.0 then
            list
        else
            match list with
            | [] ->
                []
            | _ :: tail ->
                list_drop (skippedCount -. 1.0) tail
let rec list_take_and_reverse_prepend_to : 'a list -> int -> 'a list -> 'a list =
    fun soFar toTakeCount list ->
        if toTakeCount <= 0 then
            soFar
        else
            match list with
            | [] ->
                soFar
            | head :: tail ->
                list_take_and_reverse_prepend_to
                    (head :: soFar)
                    (toTakeCount - 1)
                    tail
let list_take : float -> 'a list -> 'a list =
    fun toTakeCount list ->
        List.rev
            (list_take_and_reverse_prepend_to
                []
                (Float.to_int toTakeCount)
                list
            )
let list_sort : 'a list -> 'a list  =
    fun list -> List.sort compare list
let list_sortWith : ('a -> 'a -> basics_Order) -> 'a list -> 'a list  =
    fun elementCompare list ->
        List.sort (fun a b -> basics_orderToInt (elementCompare a b)) list
let list_range : float -> float -> float list =
    fun lowest highest ->
        List.init
            (Float.to_int (Float.sub highest lowest) + 1)
            Int.to_float
let list_intersperse : 'a -> 'a list -> 'a list =
    fun sep list ->
        match list with
        | [] -> []
        | listHead :: listTail ->
            List.fold_right
                (fun x soFar -> x :: sep :: soFar)
                listTail
                [ listHead ]
let list_foldl
    : ('a -> 'state -> 'state)
    -> 'state
    -> 'a list
    -> 'state =
    fun reduce initialState list ->
        List.fold_left
            (fun soFar element -> reduce element soFar)
            initialState
            list
let list_foldr
    : ('a -> 'state -> 'state)
    -> 'state
    -> 'a list
    -> 'state =
    fun reduce initialState list ->
        List.fold_right reduce list initialState
let list_sum : float list -> float =
    fun numbers -> List.fold_left Float.add 0.0 numbers
let list_product : float list -> float =
    fun numbers -> List.fold_left Float.mul 1.0 numbers
let list_repeat: float -> 'a -> 'a list =
    fun count element ->
        List.init (Float.to_int count) (fun _ -> element)

let string_repeat: float -> string -> string =
    fun count segment ->
        String.concat ""
            (list_repeat count segment)
let string_slice : float -> float -> string -> string =
    fun startIndexInclusive endIndexExclusive string ->
        let realStartIndex =
            if startIndexInclusive < 0.0 then
                String.length string + Float.to_int startIndexInclusive
            else
                Float.to_int startIndexInclusive
        and realEndIndexExclusive =
            if endIndexExclusive < 0.0 then
                String.length string + Float.to_int endIndexExclusive
            else
                Float.to_int endIndexExclusive
        in
        String.sub
            string
            realStartIndex
            (realEndIndexExclusive - 1 - realStartIndex)
let string_fromChar : char -> string =
    fun char -> String.make 1 char
let string_cons : char -> string -> string =
    fun newHead tail ->
        String.cat (string_fromChar newHead) tail
let string_toInt : string -> float option =
    fun string ->
        Option.map Int64.to_float
            (Int64.of_string_opt string)
let string_concat : string list -> string =
    fun strings -> String.concat "" strings
let string_filter : (char -> bool) -> string -> string =
    fun shouldBeKept string ->
        String.of_seq
            (Seq.filter shouldBeKept (String.to_seq string))
let string_lines : string -> string list =
    fun string ->
        List.map
            (fun lineWithPotentialCarriageReturns ->
                string_filter (fun char -> char <> '\r')
                    lineWithPotentialCarriageReturns
            )
            (String.split_on_char '\n' string)
let string_foldl
    : (char -> 'state -> 'state)
    -> 'state
    -> string
    -> 'state =
    fun reduce initialState list ->
        String.fold_left
            (fun soFar element -> reduce element soFar)
            initialState
            list
let string_foldr
    : (char -> 'state -> 'state)
    -> 'state
    -> string
    -> 'state =
    fun reduce initialState list ->
        String.fold_right reduce list initialState
let string_reverse : string -> string =
    fun string ->
        String.of_seq
            (List.to_seq
                (List.rev (List.of_seq (String.to_seq string)))
            )
let string_right  : float -> string -> string =
    fun takenElementCount string ->
        String.sub string
            (String.length string - Float.to_int takenElementCount - 1)
            (Float.to_int takenElementCount)
let string_left : float -> string -> string =
    fun takenElementCount string ->
        String.sub string 0 (Float.to_int takenElementCount)
let string_dropRight : float -> string -> string =
    fun skippedElementCount string ->
        String.sub string
            0
            (String.length string - Float.to_int skippedElementCount)
let string_dropRight : float -> string -> string =
    fun skippedElementCount string ->
        String.sub string
            (Float.to_int skippedElementCount - 1)
            (String.length string - Float.to_int skippedElementCount)

let format_test : string -> string = Fun.id    

