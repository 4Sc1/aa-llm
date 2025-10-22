# Uses OpenAI API to determine sentiment of full-text event entries.
# 
# Input: semicolon-separated CSV file 
# - column: event_corrected_de
# 
# Output: semicolon-separated CSV file
# - columns: event_sentiment
#
# Author: David Levi Tekampe
# University of Luxembourg

import pandas as pd
import os
import openai
from openai import OpenAI

openai_api_key = '<redacted>'

client = OpenAI(api_key=openai_api_key)

file_path = '<path to input CSV file>'
df = pd.read_csv(file_path, low_memory=False, sep=";")

def categorize_event(text):
    if pd.isna(text) or text.strip() == "":
        return text
    try:
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a clinical psychologist. Your patients were asked to list, for each hour of their day, what the main event in that hour was. Usually, patients would state their activity. When in doubt, always interpret a statement as a description of what they were doing. Sometimes they also describe their inner states or events that took place in their environment, or they just name a location that they are at or are going to. I will give you a statement of a patient, and I will ask you to categorize and analyse the sentiment of that statement. You can select from 4 sentiments. These 4 sentiments are: 1. Positive: for all statements you can identify as positive, 2. Negative: for all statements you can identify as negative, 3. Neutral: for all statements that are neutral, and 4. None: for when you cannot identify a statement's sentiment. Now, please tell me the sentiment of the following statement. Please only respond to me with the name of the sentiment (without its number). Nothing else."},
                {"role": "user", "content": text}
            ],
            temperature=0.7,
            max_tokens=4,            
            timeout=60
        )
        sentiment = response.choices[0].message.content
        return sentiment
    except Exception as e:
        print("Error: ", e)
        return "ERROR!"

dest_file = '<path to output CSV file>'

if os.path.exists(dest_file):
    os.remove(dest_file)

for index, row in df.iterrows():
    try:
        sentiment = categorize_event(row['event_corrected_de'])

        df.at[index, 'event_sentiment'] = sentiment
        
        df.iloc[[index]].to_csv(dest_file, mode='a', header=not os.path.exists(dest_file), index=False, sep=";")
    except Exception as e:
        print("Error: ", e)
