
# Uses OpenAI API to translate full-text entries from German to English
# 
# Input: comma-separated CSV file 
# - columns: skill_corrected_de, event_corrected_de
# 
# Output: semicolon-separated CSV file
# - columns: skill_en, event_en
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
df = pd.read_csv(file_path, low_memory=False)

def translate_german_to_english(text):
    if pd.isna(text) or text.strip() == "":
        return text
    try:
        response = client.chat.completions.create(
            model="gpt-3.5-turbo",
            messages=[
                {"role": "system", "content": "You are a bilingual model fluent in both German and English. It is important that you understand that the German text describes what a person does. So, if you get a German text that does not consist of subject, verb, and object, assume that the German word is a verb. For example: 'Duschen.' will mean that the person who wrote this is taking a shower, not the plural of the noun 'Dusche', which would mean 'showers'. Also, do not add anything to the text. Just translate to English."},
                {"role": "user", "content": text}
            ],
            temperature=0,
            max_tokens=256,            
            timeout=45
        )
        translated_text = response.choices[0].message.content
        return translated_text
    except Exception as e:
        print("Error: ", e)
        return "ERROR!"

dest_file = '<path to output CSV file>'

if os.path.exists(dest_file):
    os.remove(dest_file)

for index, row in df.iterrows():
    translated_skills = translate_german_to_english(row['skill_corrected_de'])
    translated_events = translate_german_to_english(row['event_corrected_de'])

    df.at[index, 'skill_en'] = translated_skills
    df.at[index, 'event_en'] = translated_events

    df.iloc[[index]].to_csv(dest_file, mode='a', header=not os.path.exists(dest_file), index=False, sep=";")
