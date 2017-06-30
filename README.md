# carpooling
App for parents who taking their children to kindergarten. It should help them to contact eachother and could be first step for sharing cars and carpooling.
Application uses Play Framework, Mongo Database and Nominatim(Open Street Maps).


Works in still progress.

Current funcionality:
- create kindergarten with its name and location
- create user withs its name, location, number of free seats in the car and kindergarten where their children attending
- delete user
- send request to other users from the same kindergarten for carpooling
- replay for request (and rejectrequest) what makes users grouped (requested user's group and replaying user's group become connected)
- show all users (groups and singles) from one kindergarten: name, surname, address, email, geoposition.

To do:
- possibility to move out the group
- make correlation between free seats in car and creating groups. 
