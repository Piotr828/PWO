# PWO++
<hr>
W razie potrzeby projekt będzie aktualizowany. Zgłaszanie issues lub pull requestów jest mile widziane.
<hr>

## Składnia
Operacje arytmetyczne
```
> 2/3
>> 0.6666666666666666
> 2 + 3
>> 5
> 2 - 3
>> -1
> 3 % 2
>> 1
> 3 ^ 2
>> 9
> pi
>> 3.141592653589793115997963468544185161590576171875
> e
>> 2.718281828459045090795598298427648842334747314453125

```
Operacje logiczne
```
> True || 3 < 2
>> True
> False && 1 = 1 // Uwaga! = jest operatorem porównania. Przypisanie to x := 1
>> False
> !False
>> True
```


Instrukcja warunkowa:
```
    if n <= 1 then
        // code
    else
        // code
    fi // lub end
```


Pętla While
```
while i * i <= n do
    if A[i] = 0 then
        j := i * i
        
        while j <= n do
            A[j] := 1
            j := j + i
        end
    fi
    i := i + 1
end

```

Pętla for 
```
x := 1.0000001
y := 1.0
sum := 0.0

for i from 1 to 10 do
    x := x * 1.00001
    y := y * 1.00001
end

/*
dostępny jest również wariant downto
*/

wynik := x - y
Return(wynik) // Nawiasy są obowiązkowe ale dopuszczalne jest też return()
```

Rzutowanie

```
> (f8) (1/3)
>> 0.25

(f3d) (1/3)
>> 0.333

(u3) (2026)
>> 2

(i32) (10^4)
>> 10000

```

Funkcje
```
fn fibo(n)
    if n <= 1 then
        Return(n)
    else
        Return(fibo(n - 1) + fibo(n - 2))
    fi
end

// Wywołanie:
Return(fibo(7))
```

Funkcje anonimowe
```
> dodaj1 := |x| -> x+1
>> fn(x)
> dodaj1(2)
>> 3

```
Biblioteki
```
use std // lub nazwa pliku bez rozszerzenia .pwo w katalogu .
```
```
use1 std // zapobiega ponownemu importowaniu pliku
```

Identyczne działanie mają kolejno `include` oraz `include_once`


## Biblioteka std
```use std``` to jedyny import, który nie wymaga żadnego dodatkowego pliku. Biblioteka zawiera definicje funkcji znane z list zadań 2026, 2025 oraz wybranych egzaminów. W przypadku funkcji ograniczonych przez aktualny rok (dynamicznie aktualizowany na podstawie kalendarza) lub wartości, ograniczenia zostały zwykle podtrzymane ale stworzono pozbawione ich wersje \_f\_ ()
```
> use std
>> 0
> sin(3 * pi)
>> 0
> _sin_(3*pi)
>> _sin_(3*pi)
>> 0.000000000012365711632360441

```
### Informacje dodatkowe
* Średniki na końcu linii są dopuszczalne ale nie są obowiązkowe
* Dopuszczalne rzutowania bitowe w systemie zmiennoprzecinkowym to f64, f32, f16, f8. Rzutowania dziesiętne (zaokrąglanie do podanej liczby cyfr dziesiętnych po przecinku) oraz wszystkie rzutowania całkowitoliczbowe mogą być arbitralne. W szczególności dopuszczalne jest `(i2) (2^2)`
## Kolorowanie składni
Biały - Cyfry wiarygodne (Interpreter ufa, że jawne rzutowanie jest oczekiwane i NIE zaznacza powstałych w jego wyniku błędów)

* Czerwony - cyfry, które prawdopodobnie zawierają szum numeryczny

* Żółty (na ostatnim miejscu) - rozwinięcie jest poprawne na wszystkich miejscach ale zostało ucięte

* Fioletowy - Zmiene logiczne (same operacje logiczne nie podlegają klasycznym błędom numerycznym)
<hr>
<small>Znaczna część składni oraz nazwa języka została zaczerpnięta z list zadań oraz egzaminów dr. hab. Pawła Woźnego</small>