<?php

namespace App\Http\Controllers;

use Illuminate\Http\Request;

use Illuminate\Support\Facades\Auth;

use App\Order;

use \Datetime;

class OrderController extends Controller
{

    public function __construct(){
        $this->middleware("role:Consumer", ['only'=> ['store']]);
    }

    /**
     * Store a newly created resource in storage.
     *
     * @param  \Illuminate\Http\Request  $request
     * @return \Illuminate\Http\Response
     */

    // function to create a new Order
    public function store(Request $request)
    {
        $user = Auth::user();
        $this->validate($request, [
            'dish' => 'required | max:30',
            'price' => 'required | numeric | min:1',
            'rest_id' => 'required | exists:Users,id',
        ]);
        $order = new Order();
        $order->user_id = Auth::user()->id;
        $order->dish = $request->dish;
        $order->price = $request->price;
        $order->orderDate = new DateTime("now");
        $order->rest_id = $request->rest_id;
        $order->created_at = new DateTime("now");
        $order->updated_at = new DateTime("now");
        $order->save();

        return view('order')->with('order', $order)->with('user', $user);
    }

    /**
     * Display the specified resource.
     *
     * @param  int  $id
     * @return \Illuminate\Http\Response
     */

    // function to show the orders of a single Restaurant (User)
    public function show($id)
    {
        $user = Auth::user();
        $orders = Order::whereRaw("rest_id == $id")->join('users', 'users.id', '=', 'orders.user_id')->select('users.name', 'orders.dish', 'orders.orderDate')->paginate(5);
        return view('restaurant_orders')->with('orders', $orders)->with('user', $user);
    }
}
