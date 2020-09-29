<!-- view to display my documentation for Assignment 2 Web Application Development -->

@extends('layouts.app')

@section('title')
    Documentation
@endsection

@section('content')
<div id = "documentation">
        <div id = "content">
            <h1>Documentation: </h1>
            <table id = "doctable">
                <col>
                <tr id = "heading">
                    <td><h3>Entity Relationship Diagram: </h3></td>
                </tr>
                <tr>
                    <td><img src="{{asset("images/ER Diagram.PNG")}}" width="60%" height="500" alt="ER Diagram"></td>
                </tr>
                <tr>
                    <td><h3>Database Implementation Details: </h3></td>
                </tr>
                <tr>
                    <td>
                        When designing my Database, I decided to keep Orders independant of Dishes (no link). The reason for this was because I knew a Dish could be
                        deleted at a later date (which would break the link to each Order it is linked to), or have its fields modified by the resaurant owner (such as
                        the price) - which would lead to incorrect statistics / order information. 
                        Through having the Order table not linked to the Dish table - it meant the details of the order are retained regardless of what happens to 
                        Restaurant dishes.
                        I used the Cart_Items table to link Consumers (Users) with Dishes. This meant Consumers would be able to place an item in their cart, and if they
                        left the website / came back at a later date - the details of their cart would be retained. It also made it easier for processing a purchased cart 
                        (adding a new Order for each item, then deleting the corresponding Cart_Item after it has been made into an Order).
                        I linked the Restaurants (Users) to Dishes, making it easy to use Laravel ORM to retrieve the Dishes of each Restaurant, and also 
                        confirm that a Restaurant was the owner of a Dish (which was important for determining whether the current User had the relevant permissions
                        to be able to edit / delete a Dish). Naturally - only the Restaurant who owns a dish should be able to edit / delete it, and can only create new 
                        Dishes for their own restaurant. 
                        Lastly - when designing my database I added an Approval column to the User, which is given a default value of FALSE when a new User is registered. 
                        Even though the field is irrelevant for the Consumers / Administrator, it was a very effective way for checking the Approval status of restaurants. 
                    </td>
                </tr>
                <tr id = "heading">
                    <td><h3>What I was able to Complete: </h3></td>
                </tr>
                <tr id = "data">
                    <td>
                    <h4>Requirements: </h4>
                        I was able to fulfill all functional basic / advanced requirements of the Assignment with the exception of User Registration Redirection (Login / Logout Redirection was completed). 
                        The requirements I completed are listed below:
                        <br><br>
                        <h4>Basic Requirements Completed: </h4>
                        <ol>   
                            <li>Users can register with this website. When registering, users must provide their name, email, password, and address.  Furthermore, users must register as either a: a. Restaurant, or b. Consumer.</li>
                            <li>Registered users can login. Users should be able to login (or get to the login page) from any page. A logged in user should be able to log out.  </li>
                            <li>Only the restaurant users can add dishes to the list of dishes sold by his/her restaurant. They can also update and delete existing dishes. A dish must have a name and a price. A dish name must be unique. A price must be a positive value. </li>
                            <li>All users (including guests) can see a list of registered restaurants. They can click into any restaurant to see the dishes this restaurant sells. </li>
                            <li>The list of dishes should be paginated with at most 5 dishes per page. </li>
                            <li>(Single purchase) Only consumers can purchase a dish. Since we do not deal with payment gateways in this course, when user clicks on purchase, we simply assume the payment is successful, and save the purchase order in the database. Then it will display the dish purchased, the price, and the delivery address (which is the consumer’s address) to let the user know that the purchase is successful. </li>
                            <li>A restaurant (user) can see a list of orders customers have placed on his/her restaurant. An order should consist of the name of the consumer, that dish (name) that was ordered, and the date that the order was placed. </li>
                        </ol>
                        <br>
                        <h4>Advanced Requirements Completed: </h4>
                        <ol>
                            <li>When restaurant users add a new dish, the dish name must be unique for that restaurant, not across restaurants. This is an extension of requirement 3. </li>
                            <li>When restaurant users add a dish, s/he can upload a photo for that dish. This photo will be displayed when this dish displayed. </li>
                            <li>In addition to requirement 6 (single purchase), consumers can add multiple dishes to a cart, see the contents in the cart, the cost of this cart (the sum of all dishes), remove any unwanted dishes, before purchasing these dishes.  Once purchased, the cart will be emptied. </li>
                            <li>There is a page which lists the top 5 most popular (most ordered) dishes in the last 30 days. </li>
                            <li>Restaurants can view a statistic page which shows the sales statics for that restaurant. This page shows: 
a. The total amount of sales (in dollar value) made by this restaurant. b. The weekly sales total (in dollar value) for the last 12 weeks, i.e. there should be a sales total for each of the last 12 weeks. </li>
                            <li>There is another user type called administrator. There is only 1 administrator which is created through seeder. The purpose of administrator is to approve new restaurant (users). After a new restaurant user (account) is registered, s/he cannot add/remove dishes from his/her restaurant until this account is approved by the administrator. There is a page where the administrator can go to see a list of new restaurant accounts that require approval, and to approve these accounts. </li>
                        </ol>
                        <br>
                            <h5>In addition to the functional requirements of the website, I followed all Technical Requirements expected for the Assignment.</h5>
                        <br>
                        <h4>Extras Completed: </h4>
                        <h5>I implemented the following extras into my Assignment website: </h5>
                        <ol>
                            <li>I added pagination to the Consumer Cart and the Orders for a Restaurant</li>
                            <li>I created custom Validation rules for checking the role of a User, checking that a Restaurant owns a Dish, and checking that a User is a Restaurant</li>
                            <li>I used a Table for the Cart - allowing the Cart details to be preserved for each User if they leave the website and return at a later stage</li>
                            <li>I enabled Consumers to order Dishes from the Most Popular Dishes (rather then having to navigate to the Restaurant where the Dish is located)</li>
                            <li>I added friendly GUI notifications to all views where appropriate (for example - if their are no restaurants, or if a cart is empty etc) </li>
                        </ol>
                        <br><br>
                        <h4>Peer Review Reflection: </h4>
                        <p>
                            I participated in Peer Review each week, and after reflecting on my experience of Peer Review I have mixed feelings towards its success. I found that reflecting on my own content / implementation to another student useful for retaining the skills taught within the course and building a better understanding, but unfortunately a lot of the other students throughout the course didn’t really care about the Peer Review process – meaning it was difficult to find students who wanted to go over their own implementation / content in detail.
A lot of students were either ignoring the Peer Review process or gave sarcastic details of their implementation (due to them either not completing the weekly milestone or thinking the Peer Review process was a joke). I improved the situation by targeting the students who were engaging in the content / putting effort in from week to week – which helped improve the feedback given on my own content, but also meant I was able to learn from other students a bit more (as it was these student who tended to have something to say about their own work). I was interested to see how other students were completing weekly activities / assignments – because it helped me to learn different approaches / new skills that I could then utilize within my own work. 
I think in the future the Peer Review process would be better if there were more students engaging in the content / taking the Peer Review process seriously, because there would be a wider array of different implementations / feedback. However, I still appreciated the Peer Review process and was able to gain value from it despite the circumstances described above.   

                        </p>
                    </td>
                </tr>
            </table>
        </div>
    </div>
@endsection