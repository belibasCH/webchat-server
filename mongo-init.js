db.createUser(
    {
        user: "webchat",
        pwd: "webchat",
        roles: [
            {
                role: "readWrite",
                db: "webchat",
            }
        ]
    }
);