type ('a, 'e) either =
  | Ok of 'a
  | Error of 'e

type ('a, 'e) result = ('a, 'e) either
