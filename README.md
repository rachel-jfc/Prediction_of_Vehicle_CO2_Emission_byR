# Prediction of Vehicle CO2 Emission by R 

## Data
Fuel consumption ratings

https://open.canada.ca/data/en/dataset/98f1a129-f628-4ce4-b24d-6f16bf24dd64

Datasets provide model-specific fuel consumption ratings and estimated carbon dioxide emissions for new light-duty vehicles for retail sale in Canada.

To help you compare vehicles from different model years, the fuel consumption ratings for 1995 to 2014 vehicles have been adjusted to reflect the improved testing that is more representative of everyday driving. Note that these are approximate values that were generated from the original ratings, not from vehicle testing.

For more information on fuel consumption testing, visit: https://www.nrcan.gc.ca/energy-efficiency/transportation-alternative-fuels/fuel-consumption-guide/understanding-fuel-consumption-ratings/fuel-consumption-testing/21008.

To compare the fuel consumption information of new and older models to find the most fuel-efficient vehicle that meets your everyday needs, use the fuel consumption ratings search tool at https://fcr-ccc.nrcan-rncan.gc.ca/en.


## Model Evaluation 

- Adjusted R-squared: 0.9005
- RMSE: 19.07
- MAE: 14.13

## Results

- Luxury Viehichles tend to have lareger CO2 emission.
- Vans and pickup trucks tend to have more CO2 emission compared to other vehicle classes.

## Conclusion
The predictive model was able to show accurate prediction based on significant variable selection. Variables such as make, model, class, cylinder, engine size, transmission and fuel type showed a strong relationship with CO2 emissions. The results can be effectively shared with consumers so that they can predict the CO2 emission of all the vehicles available for purchase within Canada and make an informed decision. 
