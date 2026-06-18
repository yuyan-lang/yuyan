Equality Checking:


1. Free Vars

```
bool : {type | [] -> [bool] } |- bool : {type | [] -> [bool]}
--------------------------------------------------------------
bool : {type | [] -> [bool] } |- bool = [bool] : type
```


2. Coercion of Static Extents

```
 bool : {type | [] -> [bool] }, false : { <| [bool] |> | [] -> [false]} |- 
 
 
 |-  false : { <| bool |> | [] -> [false]}
-----------------------------------------------------------------------------------------------------
 |- out([false], false) : <| bool |>
```