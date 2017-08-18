# carpooling
App for parents who taking their children to kindergarten. It should help them to contact eachother and could be first step for sharing cars and carpooling.
Application uses Play Framework, Mongo Database and Nominatim(Open Street Maps).


Works in still progress.

Current funcionality:
- create kindergarten with its name and location
- create user withs its name, location, number of free seats in the car and kindergarten where their children attending
- delete user
- send request to other users from the same kindergarten for carpooling. It depends on if there are enought space in user's cars.
- replay for request (and reject request) what makes users grouped (requested user's group and replaying user's group become connected)
- show all users (groups and singles) from one kindergarten: name, surname, address, email, geoposition.
- send message to timeline
- check timeline with messages from other users

To do:
- add joda date to class Message
- add filtering messages
