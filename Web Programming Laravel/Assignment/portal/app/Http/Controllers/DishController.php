<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use Illuminate\Validation\Rule;

use Illuminate\Support\Facades\Auth;

use App\Dish;
use App\User;
use App\Order;

use \Datetime;

use Illuminate\Support\Facades\DB;

class DishController extends Controller
{
    public function __construct(){
        $this->middleware("role:Restaurant", ['only'=> ['create']]);
        $this->middleware('dishes', ['only'=> ['edit', 'update', 'delete']]);
    }

    /**
     * Display a listing of the resource.
     *
     * @return \Illuminate\Http\Response
     */

    // function to return / show the 5 most popular dishes from the past 30 days 
    public function index()
    {
        $user = Auth::user();
        $dishes = Dish::join('orders', 'dishes.name', 'LIKE', 'orders.dish')->select('dishes.name', 'dishes.price', 'dishes.image', 'dishes.user_id', DB::raw("Count(*) as ordered"))->whereRaw("orders.orderDate > DATETIME('now', '-30 days')")->groupBy('dishes.name')->orderBy('ordered', 'DESC')->limit(5)->get();
        return view('popular_dishes')->with('dishes', $dishes)->with('user', $user);
    }

    /**
     * Show the form for creating a new resource.
     *
     * @return \Illuminate\Http\Response
     */

     // function to show the page where a restaurant (user) can enter details for a new dish
    public function create()
    {
        $user = Auth::user();
        return view('create_dish')->with('user', $user);
    }

    /**
     * Store a newly created resource in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @return \Illuminate\Http\Response
     */

    // function to store a newly created dish
    public function store(Request $request)
    {
        $user = Auth::user();
        $this->validate($request, [
            'name' => ['required', 
                        'max: 30',
                        'unique:dishes,name,NULL,id,user_id,'.$user->id.'',
            ],
            'price' => ['required',
                        'numeric',
                        'min: 1'
            ],
            'image' => ['image','nullable','mimes:png,jpeg,jpg,gif','max:2048']
        ]);
        $dish = new Dish();
        if($request->file('image')){
            $image = $request->file('image');
            $new_name = rand() . '.' . $image->getClientOriginalExtension();
            $image->move(public_path('images'), $new_name);
            $dish->image = $new_name;
        } else {
            $dish->image = '';
        }
        $dish->name = $request->name;
        $dish->price = $request->price;
        $dish->user_id = $user->id;
        $dish->created_at = new DateTime("now");
        $dish->updated_at = new DateTime("now");
        $dish->save();

        return redirect("/restaurant/$dish->user_id");
    }

    /**
     * Show the form for editing the specified resource.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */

    // function to show a page where a restaurant (user) can edit the details of a dish
    public function edit($id)
    {
        $user = Auth::user();
        $dish = Dish::find($id);
        return view('edit_dish')->with('dish', $dish)->with('user', $user);
    }

    /**
     * Update the specified resource in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */

    // function that updates the fields of a dish that has been updated
    public function update(Request $request, $id)
    {
        $user = Auth::user();
        $this->validate($request, [
            'name' => ['required', 
                        'max: 30',
                        'unique:dishes,name,'.$id.',id,user_id,'.$user->id.'',
            ],
            'price' => ['required',
                        'numeric',
                        'min: 1'
            ],
            'image' => ['image','nullable','mimes:png,jpeg,jpg,gif','max:2048']
        ]);
        $dish = Dish::find($id);
        if($request->file('image')){
            $image = $request->file('image');
            $new_name = rand() . '.' . $image->getClientOriginalExtension();
            $image->move(public_path('images'), $new_name);
            $dish->image = $new_name;
        } else {
            $dish->image = '';
        }
        $dish->name = $request->name;
        $dish->price = $request->price;
        $dish->updated_at = new DateTime();
        $dish->save();
        return redirect("/restaurant/$dish->user_id");
    }

    /**
     * Remove the specified resource from storage.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */

    // function to delete a dish from the database
    public function destroy($id)
    {
        $dish = Dish::find($id);
        $restaurant = $dish->user_id;
        $dish->delete();
        return redirect("/restaurant/$restaurant");
    }
}
