```
  ██████==▄████▄===▄▄▄=======██▓=====██▓███===██▓====
▒██≡≡≡≡▒≡▒██▀≡▀█≡≡▒████▄≡≡≡≡▓██▒≡≡≡≡▓██░≡≡██▒▓██▒≡≡≡≡
░=▓██▄===▒▓█====▄=▒██==▀█▄==▒██░====▓██░=██▓▒▒██░====
≡≡▒≡≡≡██▒▒▓▓▄≡▄██▒░██▄▄▄▄██≡▒██░≡≡≡≡▒██▄█▓▒≡▒▒██░≡≡≡≡
▒██████▒▒▒=▓███▀=░=▓█===▓██▒░██████▒▒██▒=░==░░██████▒
▒.▒▓▒.▒.░░.░▒.▒..░.▒▒...▓▒█░░.▒░▓..░▒▓▒░.░..░░.▒░▓..░
 ░.▒..░.░..░..▒...`.░...▒▒.░░.░.▒..░░▒.░..`..░.░.▒..░
   ░..░.`░..`..`.....`..▒.`░..░.░..`░░..`.....`░.░...
      ░..░..░..markets..░...`..`.░..░.`..lisp...`░..░
            ░.............mayhem..`..`............`..
```

# Legal-_ease_

The software is provided devoid of warantee, guarranty, liesense, and noncence:
"do as thou wilt".

# Abstract

ScalpL *will be* an engine for expressing arbitrarily intricate policies for
placement and removal of offers in asset exchanges; today, it *is* hardcoded
as a market maker.

# Exchanges

Three Bitcoin exchanges are supported: [Bitfinex](https://www.bitfinex.com),
[BTC-e](https://www.btc-e.com), and [Kraken](https://www.kraken.com). More is
in the works; contact Adlai on Freenode for details and inquiries.

# Mockumentation

Documentation currently takes the form of [penne bolognese]
(http://georgik.sinusgear.com/2011/03/07/spaghetti-lasagna-and-raviolli-code/);
translating this to natural language is a slow process, second in
priority to refactoring the dish and evolving its phenotype.

# Excuses and Explanations

* Kevin Simler's [thoughts on wealth]
(http://www.meltingasphalt.com/wealth-the-toxic-byproduct/), specifically parts
Ⅱ-Ⅳ (the "Congolese Trading Window"), outline the ideological basis for ScalpL;

* Christ Stucchio's [HFT Apology]
(http://www.chrisstucchio.com/blog/2012/hft_apology.html) outlines the mechanics
of automated market making (ie, real world Congolese Window Trading);

* Peter Seibel's [introduction]
(http://www.gigamonkeys.com/book/introduction-why-lisp.html) to the excellent
[_Practical Common Lisp_](http://www.gigamonkeys.com/book/) lays out the
factors behind the unconventional language choice; the book itself lights the
larval lisper's quest; and

* Timothy B Lee's [cards](http://www.vox.com/cards/bitcoin/what-is-bitcoin)
summarize the Bitcoin phenomenon for the unfamiliar, purely as enrichment;
though catalyzed by cryptocurrency, ScalpL itself is agn-_asset_-ic.

# Current Status

## Taming the Beast

The hardcoded volatility harvesting algorithm obeys several parameters:

* _fund-factor_ indicates the largest total fraction of the account funds (from
either asset) which should ever be offered for trade.

* _resilience-factor_ affects how deeply offers penetrate into the order book;
the factor is multiplied by the recent maximal trade volume.

* _targeting-factor_ is part of a negative feedback loop used to maintain a
desired balance between the traded assets, although it is not a hard limit;
persistent markets can elude this factor's rebalancing effect indefinitely.

* _skew-factor_ controls the *non*linearity of the feedback loop.

* _cut_ controls the tightness of placed offers; raising it yields a more
profitable spread, at the cost of reduced execution volume.

## Nature of the Beast

* Harvesting progresses best when the market moves harmoniously with the
configured parameters: for example, the most profitable targeting for a market
which trends sideways will be equal allocation.

* Allocation too far in the "wrong" direction other results in the harvester
accumulating the asset unloaded by other market participants, eventually getting
"run over": the portfolio is sufficiently imbalanced to prevent further market
making. One can either wait for the trend to reverse, or rebalance at a loss.

* Poorly calibrated resilience also weakens returns: if the resilience is too
high compared to current market activity, execution volume suffers; too low, and
the increased volume is _only_ beneficial when the market moves match the
targeting feedback cycle (mismatch accelerates the "running-over").
