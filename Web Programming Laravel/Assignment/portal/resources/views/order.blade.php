<!-- view to show the order details of a recently purchase order (or Cart) -->

@extends('layouts.app')

@section('title')
    Order Details
@endsection

@section('content')
    <div class = "container">
        <div id="content">
            <h3>Order Confirmed: </h3>
            <!-- check if a single dish was purchased, or whether a Cart was processed (multiple dishes) -->
            @if(isset($order))
                <div id="media">
                    <table>
                        <col width="100">
                        <col width="100">
                        <!-- display the details of the order(s) -->
                        <tr>
                            <th>Customer: </th>
                            <th>Address: </th>
                        </tr>
                        <tr>
                            <td>{{ Auth::user()->name }}</td>
                            <td>{{ Auth::user()->address }}</td>
                        </tr>
                        <tr>
                            <td height="20"></td>
                            <td height="20"></td>
                        </tr>
                        <tr>
                            <th>Dish: </th>
                            <th>Price: </th>
                        </tr>
                        <tr>
                            <td>{{ $order->dish }}</td>
                            <td>$ {{ $order->price }}</td>
                        </tr>
                    </table>
                </div>
            @else
                @foreach($orders as $order)
                    <div id="media">
                        <table>
                            <col width="100">
                            <col width="100">
                            <tr>
                                <th>Customer: </th>
                                <th>Address: </th>
                            </tr>
                            <tr>
                                <td>{{ Auth::user()->name }}</td>
                                <td>{{ Auth::user()->address }}</td>
                            </tr>
                            <tr>
                                <td height="20"></td>
                                <td height="20"></td>
                            </tr>
                            <tr>
                                <th>Dish: </th>
                                <th>Price: </th>
                            </tr>
                            <tr>
                                <td>{{ $order->dish }}</td>
                                <td>$ {{ $order->price }}</td>
                            </tr>
                        </table>
                    </div>
                @endforeach
            @endif
        </div>
    </div>
@endsection