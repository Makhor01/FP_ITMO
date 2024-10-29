(* Функция для вычисления длины последовательности Коллатца с использованием хвостовой рекурсии *)
let collatz_length n =
  let rec aux n length =
    if n = 1 then length
    else if n mod 2 = 0 then aux (n / 2) (length + 1)
    else aux (3 * n + 1) (length + 1)
  in
  aux n 1

(* Функция для поиска числа с самой длинной последовательностью Коллатца до миллиона *)
let find_longest_chain limit =
  let rec aux current max_number max_length =
    if current = limit then max_number
    else
      let length = collatz_length current in
      if length > max_length then
        aux (current + 1) current length
      else
        aux (current + 1) max_number max_length
  in
  aux 1 1 1

(* Поиск числа с самой длинной последовательностью для чисел от 1 до 1_000_000 *)
let result = find_longest_chain 1_000_000

let () =
(* Выводим результат *)
let () =  Printf.printf "Число, начинающее последовательность с самой длинной цепочкой: %d\n" result
