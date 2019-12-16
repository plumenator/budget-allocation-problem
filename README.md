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
          "amount": 30000,
          "district": "Lakos"
        }
      ]
    },
    {
      "billName": "An Act to Construct Shelters for the Homeless",
      "funds": [
        {
          "amount": 2742,
          "district": "Palolene"
        },
        {
          "amount": 5000,
          "district": "Southern Palolene"
        },
        {
          "amount": 1000,
          "district": "Lakos"
        }
      ]
    },
    {
      "billName": "An Act to Fund the Development of Longer-Lasting Paper",
      "funds": [
        {
          "amount": 7500,
          "district": "Palolene"
        },
        {
          "amount": 2000,
          "district": "Southern Palolene"
        },
        {
          "amount": 0,
          "district": "Lakos"
        }
      ]
    },
    {
      "billName": "An Act to Increase Retirement Benefits for Veterans",
      "funds": [
        {
          "amount": 457,
          "district": "Palolene"
        },
        {
          "amount": 5000,
          "district": "Southern Palolene"
        },
        {
          "amount": 1000,
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
1. Only 3 categories: `Defense`, `Science` and `Welfare`
1. The absence of a default means that there's no allocation for that category
1. ASCII encoded input

## Limitations
1. Due to rounding, a bill might fall short of funds even when a single
   district can fund it
1. A district's available funds may not be fully utilized when the
   caps interact with allocation proportions

## Improvements
1. Output available fund balances for districts
1. Output funding deficits for bills
1. Ensure that a bill is fully funded when possible, maybe by doing another round of allocation
1. Factor out `.Internal` modules to hide private names from the API,
   but sill making them available for testing
