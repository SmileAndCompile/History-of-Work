<!-- view to show the orders of a Restaurant (only accessible if the currently signed in User owns the Restaurant) -->

@extends('layouts.app')

@section('title')
    Restaurant Orders
@endsection

@section('content')
    <div id = content>
        <div class = "container">
            <h1>Orders: </h1>
            <div id="content">
                @if(count($orders) <= 0)
                    <h4>No Current Orders</h4>
                @else
                    <!-- display the orders for the restaurant -->
                    @foreach ($orders as $order)
                        <div id = "media">
                            <table>
                                <col width="100">
                                <col width="100">
                                <col width="100">
                                <tr>
                                    <th>Customer: </th>
                                    <th>Dish: </th>
                                    <th>Order Date: </th>
                                </tr>
                                <tr>
                                    <td>{{ $order->name }}</td>
                                    <td>{{ $order->dish }}</td>
                                    <td>{{ $order->orderDate }}</td>
                                </tr>
                            </table>
                        </div>
                    @endforeach
                @endif
                <br>
                <div class = "pagination">
                    {{ $orders->links() }}
                </div>
            </div>
        </div>
    </div>
@endsection