import requests
import json
import time

webhook_url_john = "[]"
webhook_url_dot = "[]"

def send_discord_message(content, username, webhook_url):
    payload = {
        "content": content,
        "username": username
    }
    headers = {
        "Content-Type": "application/json"
    }
    response = requests.post(webhook_url, data=json.dumps(payload), headers=headers)
    if response.status_code == 204:
        print("Message sent successfully!")
    else:
        print("Failed to send message. Error code:", response.status_code)

def read_message_content():
    with open("output.txt", "r") as file:
        lines = file.readlines()
        return [line.strip().replace('"', '') for line in lines]

def main():
    names = ["John"]
    dot_content = "."

    messages = read_message_content()

    for i, message in enumerate(messages):
        name = names[0]
        send_discord_message(message, name, webhook_url_john)
        if i < len(messages) - 1:
            send_discord_message(dot_content, "Blank", webhook_url_dot)
            time.sleep(5)  # Wait for 5 seconds before sending the next message

    print("All messages sent!")

if __name__ == "__main__":
    main()
