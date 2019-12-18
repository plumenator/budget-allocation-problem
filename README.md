# budget-allocation-problem

## Usage
```shell
$ stack exec $(basename `pwd`)-exe < sample-input.json
{
  "balances": [
    {
      "amount": 188301,
      "district": "Palolene"
    },
    {
      "amount": 137500,
      "district": "Southern Palolene"
    },
    {
      "amount": 368000,
      "district": "Lakos"
    }
  ],
  "deficits": [
    {
      "amount": 168500,
      "billName": "An Act to Construct the Great Wall of Malodivo"
    },
    {
      "amount": 31258,
      "billName": "An Act to Construct Shelters for the Homeless"
    },
    {
      "amount": 4500,
      "billName": "An Act to Fund the Development of Longer-Lasting Paper"
    },
    {
      "amount": 83543,
      "billName": "An Act to Increase Retirement Benefits for Veterans"
    }
  ],
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
1. The absence of a default means that there's no allocation for that category

## Limitations
1. Due to rounding, a bill might fall short of funds even when a single
   district can fund it

## Improvements
1. Ensure that a bill is fully funded when possible, maybe by doing another round of allocation
1. Factor out `.Internal` modules to hide private names from the API,
   but sill making them available for testing
