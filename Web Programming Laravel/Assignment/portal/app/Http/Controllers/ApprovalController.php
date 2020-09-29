<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use Illuminate\Validation\Rule;

use Illuminate\Support\Facades\Auth;

use App\User;

class ApprovalController extends Controller
{
    public function __construct(){
        $this->middleware("role:Administrator", ['only'=> ['index']]);
    }


    /**
     * Display a listing of the resource.
     *
     * @return \Illuminate\Http\Response
     */

    // function to show the currently unapproved restaurants (users), where we can approve them provided we are the Administrator
    public function index()
    {
        $user = Auth::user();
        $restaurants = User::whereRaw("role = 'Restaurant' AND approval = false")->paginate(5);
        return view('approval')->with('restaurants', $restaurants)->with('user', $user);
    }
}
