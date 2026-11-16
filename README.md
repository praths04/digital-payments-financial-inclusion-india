# Who Is Left Behind by India's Digital Payments Boom?

**Study on Financial Inclusion in India: Identifying Demographic Barriers for Digital Payments**

Logistic regression in R on the ICE 360 Household Survey 2021 (40,427 respondents, 25 states and union territories). In the survey, **76% of households said they do not use digital payments.**

**Paper:** [Research-Paper.pdf](Research-Paper.pdf) | **Code:** [RP-Code.R](RP-Code.R)

---

## How this started

During my internship on a market research and advocacy team at NowFloats, I interviewed street vendors in Hyderabad to understand whether they had benefited from digital payments after COVID-19, like the rest of the population. One coconut vendor told me he had never had access to basic banking, because he could not read, write or even sign his name.

That conversation became this project. India leads the world in digital payments, but who is being left out? A literature review showed that many studies look at why people adopt mobile payments, but few look at which demographic groups are held back, and the ones that do rely on data that doesn't represent the whole population: online surveys that mostly reached educated, higher-income users, a single region (North India), samples as small as 269 respondents, or a single demographic factor. We needed a dataset broad enough to represent India's diversity, and found one in the ICE 360 survey from PRICE. So we set out to answer the question properly.

## Question

Which demographic groups are less likely to use digital payment methods in India?

## Data and method

- **Data:** Household Survey on India's Consumer Economy and Citizen's Environment (ICE 360, 2021), from People's Research on India's Consumer Economy (PRICE), a non-profit think tank in New Delhi. 40,427 respondents (14,704 rural, 25,723 urban), ages 18 to 99. This is far larger and more demographically varied than the datasets in the earlier studies we reviewed.
- **Outcome:** whether the respondent uses digital payment methods (yes/no).
- **Model:** logistic regression with age, age squared, gender, education, occupation, social group, religion and annual household income. Survey weights were used for the descriptive statistics.
- **Tools:** R (`dplyr`, `ggplot2`, `fastDummies`, `glm`).

## Findings

These are associations in survey data, not proof of cause.

| Factor | What the model shows |
|---|---|
| Age | Adoption rises with age, then slows (inverse-U shape; age and age squared both significant) |
| Gender | Women were slightly more likely than men to use digital payments (p = 0.028) |
| Education | Matric through post-graduate was associated with higher use; primary and middle school did not differ significantly from the illiterate base group |
| Occupation | Regular salaried workers used digital payments most. Agricultural and non-agricultural labourers used them least |
| Social group | Scheduled Tribes had the lowest likelihood, followed by Other Backward Classes and Scheduled Castes |
| Income | Households with lower annual income were less likely to use digital payments than those above 25 lakhs |
| Religion | Differences were significant across groups; Sikh households were the only group below the Hindu reference |

## What this means

- Banks and policymakers do not need to prioritize gender for digital payments adoption, but they do need to make digital payments easier and more convenient for older people and people with limited education.
- Employers of labourers can adopt practices, such as digital wage payments, that bring workers into digital payments.
- More study is needed on why Scheduled Castes and Other Backward Classes are not adopting digital payments.

## Limitations

- Cross-sectional 2021 data: it shows who uses digital payments, not why, and not changes over time.
- Northeastern states are not covered by the dataset.
- Logistic regression coefficients show association, not causation.

## Files

| File | What it is |
|---|---|
| `Research-Paper.pdf` | Full paper |
| `RP-Code.R` | Data cleaning, descriptive analysis and the logistic regression |

The raw survey file is not included. To rerun the analysis, request the ICE 360 (2021) dataset from PRICE and save it as `21-Dataset.csv` in the same folder.

## Authors

N. Prathiba Reddy and Shreemayi Gunda (B.B.A. Honors, Mahindra University), under the guidance of Prof. Pranjal Chandrakar and Prof. Nilanjan Banik.
