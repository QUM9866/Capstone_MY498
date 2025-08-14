# Capstone_MY498
# Capstone MY498

This repository contains all data and code for analyzing candidate 43045's research work on celebrity social media behavior and government engagement patterns.

## Repository Structure

### Data Folder
Contains all preprocessed datasets. Raw data is not available due to ethical considerations and privacy protection.

**Important**: All celebrity IDs and names have been anonymized using consistent mapping across all datasets.

#### Datasets

**celebrity**
- Contains celebrity IDs and demographic characteristics
- Variables include gender, ethnicity, and other relevant attributes
- 208 unique celebrities total

**activity** 
- Complete collection of Weibo posts made by the 208 celebrities
- Temporal coverage: 2020-2024
- Includes all post types and engagement metrics
- Due to data size, see https://drive.google.com/file/d/1LwcZGIMycGjNHvCsSeRsQNPwlZt1AUMH/view?usp=sharing

**`repost`**
- Subset of the activity dataset
- Focuses specifically on celebrity reposts from government accounts
- Used to analyze celebrity-government interaction patterns

### Code Folder
Contains analysis scripts in both R and Python for different components of the research.

#### Python Code
Used for generating the following outputs:
- **Figures**: 1, 2, 3, 4, 10
- **Tables**: 1, 2

#### R Code  
Used for statistical analysis and generating:
- **Regression Tables**: 3, 4, 9, 10
