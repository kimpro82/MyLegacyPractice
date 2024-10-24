# [My APL Practice](/README.md#apl)

Includes languages derived from APL


### References

- Docs
  - [Wikipedia](https://en.wikipedia.org/)
    - [APL (programming language)](https://en.wikipedia.org/wiki/APL_(programming_language))
    - [A+ (programming language)](https://en.wikipedia.org/wiki/A%2B_(programming_language))
    - [J (programming language)](https://en.wikipedia.org/wiki/J_(programming_language))
    - [K (programming language)](https://en.wikipedia.org/wiki/K_(programming_language))
  - Books
    - [Wikidocs](wikidocs) > [J 언어 첫걸음](https://wikidocs.net/book/1206)
- Development Environments > [JDoodle](https://www.jdoodle.com/)
  - [Online Apl IDE](https://www.jdoodle.com/compile-apl-online)
  - [Online J Language IDE](https://www.jdoodle.com/execute-jlanguage-online)


### \<List>

- [APL / J : Calculate The Average (2024.10.25)](#apl--j--calculate-the-average-20241025)


## [APL / J : Calculate The Average (2024.10.25)](#list)

- My initial trial to code in APL and J (with [Perplexity](https://www.perplexity.ai/))
- Run with [APL 1.8](https://www.jdoodle.com/compile-apl-online) and [J 9.01.10](https://www.jdoodle.com/execute-jlanguage-online) in [JDoodle](https://www.jdoodle.com/)
- APL is an ancestor of functional programming languages released in the 1950s. It presents difficulties not only due to its strange syntax but also because **it uses special keywords not supported by modern standard keyboards**. The derivative language *J* alleviates these difficulties by using ASCII characters.
- Code and Output
  <details open="">
    <summary>APL : CalcAvg.apl</summary>

    ```apl
    avg ← {(+⌿⍵)÷≢⍵}    ⍝ Define a function to calculate the average
    avg 1 2 3 4 5       ⍝ Calculate the average of the array
    ```
    > 3
  </details>
  <details open="">
    <summary>J : CalcAvg.j</summary>

    ```apl
    avg =: +/ % #           NB. Define a function to calculate the average
    echo avg 1 2 3 4 5      NB. Calculate the average of the array
    ```
    > 3
  </details>
