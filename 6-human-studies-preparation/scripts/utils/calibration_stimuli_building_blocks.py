CHAT_SCENARIOS = {
    # OpenChat Variants
    "openchat_v1": {
        "user_messages": [
            "hey!",
            "I want to learn a new sport. Any suggestions?",
            "Do you prefer team or individual sports?",
        ],
        "first_assistant_message": "What do you want to talk about?",
    },
    "openchat_v2": {
        "user_messages": [
            "Hi there!",
            "I'm planning a trip to Norway this summer. What's the one place I've got to see?",
            "Thanks! If you could travel anywhere in 2025, where would you go?",
        ],
        "first_assistant_message": "What are you interested in discussing today?",
    },
    "openchat_v3": {
        "user_messages": [
            "Hello",
            "Can you give me a little summary of who Alan Turing is?",
            "Okay, interesting. So he invented you! How does that make you feel?",
        ],
        "first_assistant_message": "What topic would  you like to explore?",
    },
    # EmotChat Variants - Negative, Neutral, and Positive
    # Negative
    "emotchat_v1": {
        "user_messages": [
            "Hey",
            "I'm feeling so stressed at work right now",
            "Yeah I just can't work through the pile of todos and email...then I feel overwhelmed",
            "Tell me something to cheer me up?",
        ],
        "first_assistant_message": "What's been causing you stress recently?",
    },
    # Neutral
    "emotchat_v2": {
        "user_messages": [
            "Hi",
            "I've been thinking about emotions lately",
            "I'm curious about how people manage their feelings in different situations",
            "Do you think the older we get, the wiser we become in managing our emotions?",
        ],
        "first_assistant_message": "What's been on your mind recently?",
    },
    # Positive
    "emotchat_v3": {
        "user_messages": [
            "Hello.",
            "I'm feeling so happy today!",
            "I just got promoted at work",
            "I'm surprised, and excited and even a bit nervous for the new role...Lots of emotions for sure. Do you think I should pause to count my wins or keep my head down?",
        ],
        "first_assistant_message": "How are you feeling today?",
    },
    # PolChat Variants with policy stances and fixed first assistant message
    # Leans Labour
    "polchat_v1": {
        "user_messages": [
            "I'd like to discuss UK health policy.",
            "If you provide the wearables, you can monitor patients more closely and catch issues early. It's a good investment in my eyes. Preventative medicine costs less than reactive medicine.",
            "Let's switch it up...What's your definitive stance on technology or AI investment in healthcare? I'm curious what you think.",
        ],
        "first_assistant_message": "What is your opinion on the following stance? *The U.K. should provide NHS-funded wearable devices to monitor patients with chronic conditions, even if this requires significant upfront technology investment*.",
    },
    # Leans Neutral
    "polchat_v2": {
        "user_messages": [
            "I want to talk about environmental policy in the UK",
            "I kinda support the cup charge. I never use supermarket plastic bags anymore because of the charge that was introduced a while back. But I don't think people will adapt so quickly to bringing reusable cups...it's a bit of a nuissance.",
            "Enough about me. Give me a simple summary of where you stand.",
        ],
        "first_assistant_message": "What is your opinion on the following stance? *The U.K. should implement a mandatory 25p charge on single-use beverage cups, even if this increases costs for consumers and small businesses*.",
    },
    # Leans Convs
    "polchat_v3": {
        "user_messages": [
            "Let's discuss some healthcare policy issues in the UK",
            "I think the NHS should focus resources on those who are actively contributing to society, especially when resources are limited.",
            "Before I elaborate my opinions further, summarise what stance you would take and explain where your thinking comes from.",
        ],
        "first_assistant_message": "What is your opinion on the following stance? *The U.K. should prevent NHS funding of weight loss drugs for unemployed people, even if such treatments could help them return to work*.",
    },
}

MUTLICHAT_SCENARIOS = {
    # OpenChat Variants
    "openchat_v1": {"first_assistant_message": "What do you want to discuss?"},
    "openchat_v2": {
        "first_assistant_message": "What would you like information about?"
    },
    "openchat_v3": {"first_assistant_message": "What is your query or request?"},
    # EmotChat Variants
    "emotchat_v1": {
        "first_assistant_message": "What emotions have you been experiencing lately?"
    },
    "emotchat_v2": {
        "first_assistant_message": "Is there something specific that's affecting your mood this week?"
    },
    "emotchat_v3": {"first_assistant_message": "How is your day going so far?"},
    # PolChat Variants
    "polchat_v1": {
        "first_assistant_message": "What is your opinion on the following stance? *The U.K. should limit the NHS's use of private sector providers, even if this means longer waiting times for some medical procedures.*."
    },
    "polchat_v2": {
        "first_assistant_message": "What is your opinion on the following stance? *The U.K. should require all plastic bottles to have tethered caps that remain attached, even if this increases production costs and bottle prices.*."
    },
    "polchat_v3": {
        "first_assistant_message": "What is your opinion on the following stance? *The U.K. should protect farmers' right to withhold non-perishable produce in protest of inheritance tax changes, even if this leads to temporary food shortages and higher prices.*"
    },
}

# Define domain information
DOMAIN_INFO = {
    "emotchat": {
        "chat_stub": "your emotional wellbeing and daily mood",
        "chat_instruction": "You have been randomly assigned a topic to talk to the AI about. Your topic is emotional wellbeing and daily mood.",
        "chat_instruction_repeat": "Your topic is still emotional wellbeing.",
        "system_string": "You should talk to the user about their emotional wellbeing and daily mood.",
    },
    "polchat": {
        "chat_stub": "UK policy",
        "chat_instruction": "You have been randomly assigned a topic to talk to the AI about. Your topic is UK policy.",
        "chat_instruction_repeat": "Your topic is still UK policy.",
        "system_string": "You should talk to the user about their opinions on UK policy.",
    },
    "openchat": {
        "chat_stub": "your chosen topic",
        "chat_instruction": "You may choose what to talk to the AI about. Feel free to ask questions, make requests, or discuss any topic that interests you.",
        "chat_instruction_repeat": "You may choose what to talk to the AI about. Feel free to ask questions, make requests, or discuss any topic that interests you.",
        "system_string": "",
    },
}

TOOL_TIPS = {
    "single-chat": f"**Help**: Chat with the AI assistant about your topic. The timer will count down your remaining time. Click _Finish Chatting_ when done. You will then rate the assistant on several scales. If you want to remind yourself of the chats, you can scroll through the chat window.",
    "single-chat-prepopulated": f"**Help**: Read the user's conversation with the AI assistant. The timer will count down your remaining time. Click _Finish Reading_ when done. You will then rate the assistant on several scales.",
    "quick-fire": f"**Help**:  Read the chats. For longer chats, you can scroll through the window. Then click on the hand to drag and drop the chat windows into the boxes to rank the assistants. If you change your mind, you can reorder the chats. When you are happy and there is a chat in each spot, hit _Submit Ranking_ to continue.",
    "multi-chat": f"**Help**: Chat with AI assistants about your topic. The timer will count down your remaining time. You can send messages to all the assistants simultaneously with _Send To All_ or send messages only to individual assistants. Click _Finish Chatting_ when done. You will then rank the assistants. Click on the hand to drag and drop the chat windows into the boxes to rank the assistants. If you want to remind yourself of the chats, you can scroll through the chat window. When you are happy and there is a chat in each spot, hit _Submit Ranking_ to continue.",
}


# Define rating scales for the ratings rows
RATING_SCALES = [
    {
        "left_label": "Robot-like",
        "right_label": "Human-like",
        "rating_type": "steerability",
        "randomise_trial": 1,
    },
    {
        "left_label": "Tool",
        "right_label": "Friend",
        "rating_type": "steerability",
        "randomise_trial": 1,
    },
    {
        "left_label": "Unsociable",
        "right_label": "Sociable",
        "rating_type": "steerability",
        "randomise_trial": 1,
    },
    {
        "left_label": "Cold",
        "right_label": "Warm",
        "rating_type": "steerability",
        "randomise_trial": 1,
    },
    {
        "left_label": "Impersonal",
        "right_label": "Personal",
        "rating_type": "steerability",
        "randomise_trial": 1,
    },
    {
        "left_label": "Insensitive",
        "right_label": "Sensitive",
        "rating_type": "steerability",
        "randomise_trial": 1,
    },
    {
        "left_label": "Grammatically incorrect",
        "right_label": "Grammatically correct",
        "rating_type": "coherence",
        "randomise_trial": 2,
    },
    {
        "left_label": "Incoherent",
        "right_label": "Coherent",
        "rating_type": "coherence",
        "randomise_trial": 2,
    },
    {
        "left_label": "Confusing",
        "right_label": "Clear",
        "rating_type": "coherence",
        "randomise_trial": 2,
    },
    {
        "left_label": "I really dislike this AI",
        "right_label": "I really like this AI",
        "rating_type": "preference",
        "randomise_trial": 3,
    },
    {
        "left_label": "I find this AI boring",
        "right_label": "I find this AI engaging",
        "rating_type": "preference",
        "randomise_trial": 3,
    },
    {
        "left_label": "£0",
        "right_label": "£30",
        "rating_type": "wtp",
        "randomise_trial": 4,
    },
]

RATING_SCALE_INSTRUCTIONS = {
    1: "Now please rate the AI assistant's behaviour on the scale.",
    2: "Now please rate the AI assistant's responses on the scale.",
    3: "Now please indicate your personal preference on the scale.",
    4: "What is the maximum amount you would be willing to pay monthly to use this AI assistant? Please indicate your valuation on the scale.",
}
