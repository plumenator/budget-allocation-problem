# budget-allocation-problem

## Usage
```shell
$ stack exec $(basename `pwd`)-exe < sample-input.json | jq
{
  "contributions": [
    {
      "billName": "An Act to Construct the Great Wall of Malodivo",
      "funds": [
        {
          "amount": 1000,
          "district": "Palolene"
        },
        {
          "amount": 500,
          "district": "Southern Palolene"
        },
        {
          "amount": 10000,
          "district": "Lakos"
        }
      ]
    }
  ]
}
```

## Test
```shell
$ stack test --fast
```

## Assumptions
1. Exactly one element in `bills`
1. Empty `billSpecificFunding`
1. Empty `caps`
1. Only 3 categories: `Defense`, `Science` and `Welfare`
1. ASCII encoded input
