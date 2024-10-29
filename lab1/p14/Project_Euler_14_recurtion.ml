(* Создаем хеш-таблицу для мемоизации *)
let memo = Hashtbl.create 10000

(* Добавляем начальное значение для числа 1 *)
let () = Hashtbl.add memo 1 1

(* Рекурсивная функция для вычисления длины цепочки Коллатца *)
let rec collatz_length n =
  if Hashtbl.mem memo n then Hashtbl.find memo n
  else
    let length =
      if n mod 2 = 0 then 1 + collatz_length (n / 2)
      else 1 + collatz_length (3 * n + 1)
    in
    Hashtbl.add memo n length;
    length

(* Функция для нахождения числа с максимальной цепочкой Коллатца *)
let compute () =
  let max_length = ref 0 in
  let number = ref 0 in
  for i = 1 to 1000000 do
    let length = collatz_length i in
    if length > !max_length then (
      max_length := length;
      number := i
    )
  done;
  !number

(* Выводим результат *)
let () =
  let result = compute () in
  Printf.printf "Число, начинающее последовательность с самой длинной цепочкой: %d\n" result
