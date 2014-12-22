# Border Patrol Integration Tests

This module includes dummy integration test services for Border Patrol.

## Developing/Contributing

### Installation

#### Darwin

* bundle install
* god -Dbc borderpatrol.god

[God](http://godrb.com) to run 4 processes, on the following ports

 * `9081` Mock Authorization service
 * `9082` Mock downstream service A
 * `9083` Mock downstream service B
 * `9084` Mock Account Service

To stop the integration services, kill god and shotgun

pkill -9 -f shotgun; pkill -9 -f god

