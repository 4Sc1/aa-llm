# Uses OpenAI API to categorise of full-text event entries.
# 
# Input: semicolon-separated CSV file 
# - column: event_corrected_de
# 
# Output: semicolon-separated CSV file
# - columns: event_category
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
                {"role": "system", "content": "You are a clinical psychologist. Your patients were asked to list for each hour of their day what the main event in that hour was. Usually, patients would state their activity. When in doubt, always interpret a statement as a description of what they were doing. Sometimes, they also describe their inner states or events that took place in their environment. I will give you a statement from a patient and I will ask you to categorize that statement professionally. You can select from 6 main categories. These 6 categories are: 1. Social and Personal Relationships: Encompasses various interactions and activities involving personal connections and social engagements that are not work-related. Its Subcategories are: Interpersonal Interactions, Shared Activities, Family and Kinship, Friendship and Companionship, Pet and Animal Care, Romantic and Partner Relationships, Caregiving and Support, Social Outings and Gatherings, Emotional and Conflict Exchanges. 2. Daily Routines and Household Activities: Encapsulates a variety of everyday tasks, personal care, and home-related activities. Its Subcategories are: Meals and Snacks, Personal Care and Hygiene, Household Chores, Shopping and Errands, Leisure and Entertainment, Financial and Administrative Tasks, Rest and Relaxation, Moving and Relocating. 3. Work and Professional Engagements: Encompasses a wide array of activities, tasks, and interactions that are related to professional life and work environments, and education. Its Subcategories are: Work-Related Tasks and Projects, Educational and Academic Work, Professional Communication and Meetings, Workplace Dynamics and Environment, Professional Development and Training, Work-Life Balance, Work-Related Travel and Events, Professional Collaborations and Partnerships, Creative and Design Work. 4. Health and Well-being: Encompasses a wide range of topics related to physical and mental health, emotional states, medical treatments, and general well-being. Its Subcategories are: Mental Health and Emotional State, Medical Treatment and Healthcare Interactions, Physical Health and Symptoms, Therapeutic and Rehabilitation Activities, Psychological and Emotional Support, Well-being Practices and Exercises, Mood and Emotional Fluctuations, Healthcare Journeys and Visits, Coping Mechanisms and Strategies. 5. Leisure and Recreation: Covers a wide range of activities and experiences that people engage in during their free time for relaxation, enjoyment, and entertainment. Its Subcategories are: Entertainment and Media Consumption, Outdoor and Recreational Activities, Sports and Physical Activities, Travel and Excursions, Cultural and Community Events, Relaxation and Rest, Games and Hobbies, Socializing and Eating Out, Leisure Travel and Commuting, Reading and Intellectual Activities. 6. Indeterminate: Encompasses every statement that does not belong to the other 5 categories. Now, please tell me to which main category this statement belongs. Please only respond to me with the name of the main category (without its number). Nothing else."},
                {"role": "user", "content": text}
            ],
            temperature=0.7,
            max_tokens=2048,            
            timeout=60
        )
        category = response.choices[0].message.content
        return category
    except Exception as e:
        print("Error: ", e)
        return "ERROR!"

dest_file = '<path to output CSV file>'

if os.path.exists(dest_file):
    os.remove(dest_file)

for index, row in df.iterrows():
    try:
        event_category = categorize_event(row['event_corrected_de'])

        df.at[index, 'event_category'] = event_category

        df.iloc[[index]].to_csv(dest_file, mode='a', header=not os.path.exists(dest_file), index=False, sep=";")
    except Exception as e:
        print("Error: ", e)
