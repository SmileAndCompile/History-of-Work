<!-- view to show the sales statistics of a Restaurant (Total sales and weekly sales for the past 12 weeks) -->

@extends('layouts.app')

@section('title')
    Restaurant Statistics
@endsection

@section('content')
    <div id = content>
        <h1>{{$user->name}}</h1>
        <br>

        <!-- display the total sales figure for the logged in restaurant -->
        <h4>Total Sales:</h4>
        <h5>$ {{ $total_sales }}</h5>
        <br>
        <h3>Weekly Sales (Past 12 Weeks): </h3>
        <div class = "container">
            <div id="content">

                <!-- display the sales figures for each of the past 12 weeks for the logged in Restaurant -->
                @foreach ($weekly_sales as $weekly_sale)
                    <div id = "media">
                        <table>
                            <col width="100">
                            <col width="100">
                            <col width="100">
                            <tr>
                                <th>Start Date: </th>
                                <th>End Date: </th>
                                <th>Weekly Sales: </th>
                            </tr>
                            <tr>
                                <td>{{$weekly_sale[0]}}</td>
                                <td>{{$weekly_sale[1]}}</td>
                                <td>$ {{$weekly_sale[2]}}</td>
                            </tr>
                        </table>
                    </div>
                @endforeach
            </div>
        </div>
    </div>
@endsection