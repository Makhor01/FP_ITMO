(* Модуль для работы с последовательностью Коллатца *)
module Collatz = structhttps://github.com/Makhor01/FP_ITMO/edit/main/lab1/Project_Euler_14_modules
  let memo = Hashtbl.create 10000

  (* Добавляем начальное значение для числа 1 *)
  let () = Hashtbl.add memo 1 1

  (* Функция для генерации последовательности Коллатца *)
  let rec generate_sequence n =
    if Hashtbl.mem memo n then []
    else
      let next_n = if n mod 2 = 0 then n / 2 else 3 * n + 1 in
      n :: generate_sequence next_n

  (* Функция для вычисления длины последовательности и мемоизации *)
  let rec collatz_length n =
    if Hashtbl.mem memo n then Hashtbl.find memo n
    else
      let length = 1 + collatz_length (if n mod 2 = 0 then n / 2 else 3 * n + 1) in
      Hashtbl.add memo n length;
      length

  (* Функция для фильтрации последовательностей, завершившихся на 1 *)
  let filter_to_end_on_one seq =
    List.filter (fun x -> Hashtbl.mem memo x) seq

  (* Функция для свёртки и нахождения максимума *)
  let find_longest_sequence max_limit =
    let numbers = List.init max_limit (fun x -> x + 1) in
    List.fold_left
      (fun (max_len, max_num) i ->
         let length = collatz_length i in
         if length > max_len then (length, i) else (max_len, max_num))
      (0, 0)
      numbers
end

let () =
  let _, result = Collatz.find_longest_sequence 1_000_000 in
  Printf.printf "Число, начинающее последовательность с самой длинной цепочкой: %d\n" result
