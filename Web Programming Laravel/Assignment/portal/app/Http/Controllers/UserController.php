<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;
use Illuminate\Support\Facades\Auth;

use App\User;
use App\Dish;


use \Datetime;

class UserController extends Controller
{
    /**
     * Display a listing of the resource.
     *
     * @return \Illuminate\Http\Response
     */

    // function to show the available restaurants (Users) currently available on the website
    public function index()
    {
        $user = Auth::user();
        $restaurants = User::whereRaw("role = 'Restaurant'")->paginate(5);
        return view('home')->with('restaurants', $restaurants)->with('user', $user);
    }

    /**
     * Display the specified resource.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */

    // function to show the dishes of a restaurant (User)
    public function show($id)
    {
        $user = Auth::user();
        $dishes = User::find($id)->dishes()->paginate(5);
        $restaurant = User::find($id);
        return view('dishes')->with('dishes', $dishes)->with('restaurant', $restaurant)->with('user', $user);
    }

    /**
     * Update the specified resource in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */

    // function to update a Restaurants Approval Field (When a Restaurant (User) is Approved)
    public function update(Request $request, $id)
    {
        $user = User::find($id);
        $user->approval = true;
        $user->updated_at = new DateTime();
        $user->save();
        return redirect("/approval");
    }
}
