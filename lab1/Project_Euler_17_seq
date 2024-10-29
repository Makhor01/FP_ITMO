(* Определение слов для базовых чисел *)
let ones = [| ""; "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" |]
let teens = [| "ten"; "eleven"; "twelve"; "thirteen"; "fourteen"; "fifteen"; "sixteen"; "seventeen"; "eighteen"; "nineteen" |]
let tens = [| ""; ""; "twenty"; "thirty"; "forty"; "fifty"; "sixty"; "seventy"; "eighty"; "ninety" |]
let hundred = "hundred"
let thousand = "thousand"

(* Функция для преобразования числа в его текстовый эквивалент *)
let rec number_to_words n =
  if n < 10 then ones.(n)
  else if n < 20 then teens.(n - 10)
  else if n < 100 then tens.(n / 10) ^ ones.(n mod 10)
  else if n < 1000 then
    let hundreds_part = ones.(n / 100) ^ hundred in
    let remainder_part = if n mod 100 = 0 then "" else "and" ^ number_to_words (n mod 100) in
    hundreds_part ^ remainder_part
  else if n = 1000 then ones.(1) ^ thousand
  else ""

(* Функция для подсчета общего количества букв с использованием бесконечной последовательности *)
let count_letters max =
  Seq.unfold (fun n -> if n > max then None else Some (n, n + 1)) 1  (* Бесконечная последовательность от 1 до max *)
  |> Seq.map number_to_words                                         (* Преобразуем каждое число в его текстовое представление *)
  |> Seq.map String.length                                           (* Получаем длину каждой строки *)
  |> Seq.fold_left (+) 0                                             (* Суммируем длины всех строк *)

(* Запуск подсчета для чисел от 1 до 1000 *)
let result = count_letters 1000
let () = Printf.printf "Total letters: %d\n" result
