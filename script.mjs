import fs from 'fs';
const orgKey = '[redacted]';
const apiKey = '[redacted]';
import { Configuration, OpenAIApi } from "openai";

const hobbies = [
  "Photography",
  "Painting",
  "Cooking",
  "Gardening",
  "Playing a musical instrument",
  "Writing",
  "Reading",
  "Knitting",
  "Dancing",
  "Hiking",
  "Cycling",
  "Swimming",
  "Yoga",
  "Playing chess",
  "Collecting stamps",
  "Birdwatching",
  "Sculpting",
  "Pottery",
  "Fishing",
  "Playing video games",
  "Woodworking",
  "Calligraphy",
  "Sewing",
  "Origami",
  "Playing tennis"
];

const configuration = new Configuration({
  apiKey: apiKey,
});
const openai = new OpenAIApi(configuration);

for (const hobby of hobbies) {
  const systemMessage = `You are someone with a ${hobby} hobby. Please describe your hobby ${hobby} in 3 sentences.`;
  const userMessage = `Please describe your hobby ${hobby} in 3 sentences.`;

  const chatCompletion = await openai.createChatCompletion({
    model: "gpt-3.5-turbo",
    messages: [
      { role: "system", content: systemMessage },
      { role: "user", content: userMessage }
    ],
  });

  const output = chatCompletion.data.choices[0].message.content;
  const outputString = JSON.stringify(output);

  fs.writeFileSync('output.txt', outputString + '\n', { flag: 'a' });
  console.log(output);
}
