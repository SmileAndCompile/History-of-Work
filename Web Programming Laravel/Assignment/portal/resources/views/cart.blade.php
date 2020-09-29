<!-- view where a Consumer (User) can view the dishes of their Cart, and choose to delete items or purchase all items within the Cart -->

@extends('layouts.app')

@section('title')
    Cart
@endsection

@section('content')
    <div id = content>
        <h1>Cart:</h1>
        <br>
        <div class = "container">
            <div id="content">
                @if(count($cart_items) <= 0)
                    <h4>Cart is Currently Empty</h4>
                @else
                    <h3>Total Price: </h3>
                    <h4>$ {{ $price }}</h4>

                    <!-- display each item in the current Consumers cart -->
                    @foreach ($cart_items as $cart_item)
                        <div id = "media">
                            <table>
                                <col width="100">
                                <col width="100">
                                <col width="100">
                                <col width="100">
                                <tr>
                                    <th>Photo: </th>
                                    <th>Name: </th>
                                    <th>Price: </th>
                                    <th>Options: </th>
                                </tr>
                                <tr>
                                    <td><img src="{{ asset("/images/$cart_item->image")}}" alt="Photo" width=100 height=100></td>
                                    <td>{{$cart_item->name}}</td>
                                    <td>$ {{$cart_item->price}}</td>
                                    <td>
                                        <form action="{{url("cart/$cart_item->id")}}" method="Post"> 
                                            {{ csrf_field() }}
                                            {{ method_field('DELETE') }} 
                                            <input type="submit" value="Delete"> 
                                        </form>
                                    </td>
                                </tr>
                            </table>
                        </div>
                    @endforeach
                @endif
                <!-- only allow a consumer to purchase all items of a Cart if there is atleast 1 item -->
                @if(count($cart_items) > 0)
                    <form action="{{url("cart/$user->id")}}" method="Get">
                        {{ csrf_field() }} 
                        <input type="submit" value="Purchase" class="add">
                    </form> 
                @endif
                <br>
                <div class = "pagination">
                    {{ $cart_items->links() }}
                </div>
            </div>
        </div>
    </div>
@endsection