#pragma once

#include <iostream>
#include <array>

template<size_t N_ROW, size_t N_COL>
void iterate(uint64_t* __restrict a, uint64_t* __restrict b)
{
	for (size_t j1 = 0; j1 < N_COL; j1++) // first loop
		b[0 * N_COL + j1] ^= a[0 * N_COL + j1] ^ a[1 * N_COL + j1];
	
	for (size_t i = 1; i < N_ROW - 1; i++) // middle loops
		for (size_t j1 = 0; j1 < N_COL; j1++)
			b[i * N_COL + j1] ^=  a[(i - 1) * N_COL + j1] ^ a[i * N_COL + j1] ^ a[(i + 1) * N_COL + j1];

	for (size_t j1 = 0; j1 < N_COL; j1++) // last loop
		b[(N_ROW - 1) * N_COL + j1] ^= a[(N_ROW - 2) * N_COL + j1] ^ a[(N_ROW - 1) * N_COL + j1];

	for (size_t i = 0; i < N_ROW; i++) // update a
		a[i * N_COL] ^= size_t(1);
}

template<size_t N_ROW, size_t N_COL>
int gauss(uint64_t* __restrict a, bool* __restrict result)
{
	// forward
	for (size_t i = 0; i < N_ROW; i++)
	{
		auto d1 = (i + 1) >> 6;
		auto d2 = (i + 1) & 0x3fu;
		auto mask = size_t(1) << d2;

		if (i == N_ROW - 1)
			if (a[i * N_COL + d1] & mask)
				break;
			else
				return 1;

		// reduce and swap
		auto found = false;
		for (size_t k = i; k < N_ROW; k++)
		{
			if (a[k * N_COL + d1] & mask)
			{
				// reduce
				for (size_t r = k + 1; r < N_ROW; r++)
				{
					if (a[r * N_COL + d1] & mask)
						for (size_t j1 = 0; j1 < N_COL; j1++)
							a[r * N_COL + j1] ^= a[k * N_COL + j1];
				}
				// swap
				if (k > i)
					for (size_t j1 = 0; j1 < N_COL; j1++)
					{
						auto swap = a[k * N_COL + j1];
						a[k * N_COL + j1] = a[i * N_COL + j1];
						a[i * N_COL + j1] = swap;
					}
				found = true;
			}
			if (found)
				break;
		}
		if (!found)
			return 1;
	}

	// backward
	for (size_t i = N_ROW - 1; i > 0; i--)
	{
		auto d1 = (i + 1) >> 6;
		auto d2 = (i + 1) & 0x3fu;
		auto mask = size_t(1) << d2;
		for (size_t r = 0; r < i; r++)
			if (a[r * N_COL + d1] & mask)
				for (size_t j1 = 0; j1 < N_COL; j1++)
					a[r * N_COL + j1] ^= a[i * N_COL + j1];
	}

	// write result
	for (size_t i = 0; i < N_ROW; i++)
		result[i] ^= a[i * N_COL] & size_t(1);

	return 0;
}

template<size_t N>
auto solve()
{
	constexpr size_t N_ROW = N;
	constexpr size_t N_COL = N / 64u + 1u;
	
	uint64_t* a = new uint64_t[N_ROW * N_COL];
	uint64_t* b = new uint64_t[N_ROW * N_COL];
	auto result = std::array<bool, N>();
	uint64_t* c = nullptr;

	for (size_t i = 0; i < N_ROW * N_COL; i++)
		a[i] = b[i] = 0;
	for (size_t i = 0; i < N; i++)
		result[i] = 0;

	for (size_t i = 0; i < N_ROW; i++)
	{
		auto j1 = (i + 1) >> 6;
		auto j2 = (i + 1) & 0x3fu;
		a[i * N_COL + j1] = size_t(1) << j2;
		b[i * N_COL] = size_t(1);
	}

	for (size_t t = 0; t < N_ROW; t++)
	{
		iterate<N_ROW, N_COL>(a, b);
		c = a; a = b; b = c; // swap a and b
	}

	gauss<N_ROW, N_COL>(a, &result[0]);

	delete a;
	delete b;

	return result;
}

