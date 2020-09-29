<?php

/*
|--------------------------------------------------------------------------
| Web Routes
|--------------------------------------------------------------------------
|
| Here is where you can register web routes for your application. These
| routes are loaded by the RouteServiceProvider within a group which
| contains the "web" middleware group. Now create something great!
|
*/
use App\User;

Route::get('/', function () {
    $user = Auth::user();
    $restaurants = User::whereRaw("role == 'Restaurant'")->paginate(5);
    return view('home')->with('restaurants', $restaurants)->with('user', $user);
});

// route for Assignment documentation
Route::get('/documentation', function(){
    $user = Auth::user();
    return view('documentation')->with('user', $user);
});

Auth::routes();

Route::resource('restaurant', 'UserController');

Route::resource('dish', 'DishController');

Route::resource('order', 'OrderController');

Route::get('/logged_in', 'HomeController@index')->name('logged_in');

Route::get('/home', 'HomeController@index')->name('home');

Route::resource('approval', 'ApprovalController');

Route::resource('user', 'UserController');

Route::resource('cart', 'CartItemController');

Route::resource('statistics', 'StatisticsController');

