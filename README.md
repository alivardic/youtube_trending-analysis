================

## Maximizing Success on YouTube's Trending Page: Content Optimization Strategies Based on Analysis of the Top 200 Videos Over the Last Four Years

<!-- badges: start -->
<!-- badges: end -->

## 🌐 Research Question and Goal

**Research Question:** What factors contribute to the consistent success of YouTube videos that appear on the trending page in the United States?


**Project goal:** : To analyze data from each day’s top 200 trending YouTube videos from August 2020 to April 2024 and identify success factors among the most successful videos in the U.S. This analysis will focus on the following success metrics:

  - *Engagement Ratio:* The total audience interaction (sum of comments, likes, and
  dislikes) relative to the view count as recorded on the final day the video appears on the
  trending list (representing the highest engagement level captured in the dataset)
  
  - *Trending Speed:* How long a video took to reach the trending page
  
  - *Trending Retention:* How long a video remains on the trending page
  
## 📂 The Dataset
This dataset consists of data from each day’s top 200 trending YouTube videos from August 2020 to April 2024. This dataset is sourced from YouTube’s API and was updated daily during that time period. 

The dataset was found on Kaggle at `https://www.kaggle.com/datasets/rsrishav/youtube-trending-video-dataset`. The data consists of a csv with trending video data, such a views, engagement, and descriptive text fields, and a nodes json table with channel category labels.

## 📦 Mining Techniques Used
- Data cleaning and wrangling (`dplyr`, `tidyr`)
- Descriptive Statistics and Visualization (`ggplot2`)
- Sentiment analysis (`tidytext`, `syuzhet`)
- Association rule mining (`arules`)

### 1️⃣ Data Cleaning
- Combined and cleaned daily YouTube trending datasets (US_youtube_trending_data.csv).
- Normalized column names and converted dates to proper formats.
- Added categorical metadata (e.g., channel categories from US_category_id.json).

### 2️⃣ Descriptive Statistics and Visualization
**Goals and Research Questions:**
- Is there a correlation between engagement ratio, trending speed, and trending retention among trending YouTube videos?
- Who are the most successful YouTubers in the United States, as determined by their total number of unique videos that have reached the trending page, cumulative time spent trending, and average success metrics? Are these independent creators or Companies?
- What are the most successful channel categories in the United States, as determined by total number of unique videos that have reached the trending page, cumulative time spent trending, and average success metrics?
- Make Visual Diagrams, like bar charts or heatmaps, to illustrate the success distribution across creators and categories, revealing any visible trends or outliers.

### 3️⃣ Sentiment Analysis
**Goals and Research Questions:**
- What are the average success metrics depending on videos descriptive sentiments?
- Do video sentiments have a statistically significant impact on success metrics? (T-Test / ANOVA hypothesis test)
- Are trending videos more commonly positive, negative, or associated with other emotions?
- Do trending videos in certain channel categories lean to specific sentiments?
- Do different trending video titles affect or correlate with viewer sentiment (More likes or dislikes)?
- What are the overall sentiments of each Unique Video in the dataset? (Add Output to Unique.Video_DF)

### 4️⃣ Association Mining
**Goals and Research Questions:**
- Tag Association with Better Success Metrics:
  - Are certain tags (e.g., “reaction,” “official”) associated with better success metrics?
  - Are videos with a higher number of tags associated with better success metrics?
  - Do certain tags (e.g., “prank,” “DIY”) perform better in specific channel categories like music, film, news, etc.?
- Are videos published at specific times (Times of day, times of week, times of month, times of year) associated with better success metrics?
- Are videos of certain sentiments associated with better success metrics?

## ✨  Results and Analysis

Our analysis of YouTube trending videos revealed several key findings about success factors once a video reaches the trending page:

* **Descriptive Statistics:** Across 268,704 videos, the average view count was about 2.7M, with a median of ~937k. Average likes were ~131k, and comments averaged ~10k. These baseline statistics helped frame later analyses of engagement, speed to trend, and retention.

* **Sentiment Analysis:** While nearly half of trending videos had positive dominant sentiment, statistical testing suggested that sentiment **does not have a clear impact** on success metrics like trending speed, retention, or engagement. The ANOVA test indicated potential significance, but violations of normality meant results were unreliable. Creators should prioritize **content quality and community engagement** over tailoring emotional tone. This went against our hypothesis that videos that pulled on more negative content such as anger or sadness would have higher engagement metrics.

* **Association Mining:** There was **no universal strategy** found for guaranteed trending success. However, rule mining identified **behaviors to avoid** by content category—for example, comedy channels should limit tags and keep titles short, while music videos perform better with concise titles and detailed descriptions. Ultimately, success appears to rely on **consistent quality, audience loyalty, and community interaction**, rather than simple metadata adjustments.

Overall, while some practices can help avoid poor performance, there is no formula for trending success. The strongest recommendation is to **focus on creating engaging, high-quality videos that build a loyal audience**, which in turn can lead to faster trending and better retention.

## 🚨 Project Limitations
**Recommendation Limitations**
Our original goal was to provide actionable recommendations for creators on how to maximize their chances of reaching the trending page. However, the dataset only included videos that had already trended. Without data on videos that did not trend, we could not compare successful and unsuccessful videos. As a result, our insights focus on how to perform better once a video is trending, not how to reach the trending page in the first place.

**Creator vs. Company Classification**
We aimed to differentiate between independent creators and company-run channels but found no scalable way to do so. While we manually classified the top 50 channels, automated attempts (e.g., using keywords like `Official`, `VEVO`, `Productions`) had a high error rate. Future work could leverage the YouTube API to access internal metadata for accurate classification.

**Hardware Limitations**
Sentiment analysis and association mining placed heavy strain on computing resources, with some processes taking 20–30 minutes each. The full R Markdown pipeline took ~32 minutes to run. Because of these constraints, we limited our study to U.S. trending data and could not include videos from other regions or videos that never trended.

## 🚀 Future Improvements
**Use of Advanced NLP Models:** 
Incorporating large language models (LLMs) or more sophisticated NLP techniques could enhance insights on how metadata and sentiment affect success.

**Expanded Datasets:**
With more computing power, future analyses could include:

- Trending data from multiple countries/regions.

- Videos that did not trend, enabling comparisons to better understand factors that drive trending success.

**Improved Classification:** 
Using YouTube API metadata could improve differentiation between company channels and independent creators.

## 👥 Team 
**Alina Hagen:** Project lead, Association Mining, Debugging

**Joshua Motte:** Data Wrangling, Debugging

**Brodie Vancil:** Descriptive Statistics

**Sherman LaCost:** Sentiment Analysis 
