#include <stdio.h>
#include <iostream>
#include <cstring>
#include <map>

using namespace std;

#define startPosition "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1 "
#define trickyPosition "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 "
#define killerPosition "rnbqkb1r/pp1p1pPp/8/2p1pP2/1P1P4/3P3P/P1P1P3/RNBQKBNR w KQkq e6 0 1"

enum Square {
    a8, b8, c8, d8, e8, f8, g8, h8,
    a7, b7, c7, d7, e7, f7, g7, h7,
    a6, b6, c6, d6, e6, f6, g6, h6,
    a5, b5, c5, d5, e5, f5, g5, h5,
    a4, b4, c4, d4, e4, f4, g4, h4,
    a3, b3, c3, d3, e3, f3, g3, h3,
    a2, b2, c2, d2, e2, f2, g2, h2,
    a1, b1, c1, d1, e1, f1, g1, h1, noSq
};

enum Side { WHITE, BLACK, BOTH };

enum Piece { P, N, B, R, Q, K, p, n, b, r, q, k };

enum CastlingRights { WK = 1, WQ = 2, BK = 4, BQ = 8 };

const char *asciiPieces = "PNBRQKpnbrqk";

const char* notation[] = {
	"a8", "b8", "c8", "d8", "e8", "f8", "g8", "h8",
	"a7", "b7", "c7", "d7", "e7", "f7", "g7", "h7",
	"a6", "b6", "c6", "d6", "e6", "f6", "g6", "h6",
	"a5", "b5", "c5", "d5", "e5", "f5", "g5", "h5",
	"a4", "b4", "c4", "d4", "e4", "f4", "g4", "h4",
	"a3", "b3", "c3", "d3", "e3", "f3", "g3", "h3",
	"a2", "b2", "c2", "d2", "e2", "f2", "g2", "h2",
	"a1", "b1", "c1", "d1", "e1", "f1", "g1", "h1"
};

map<char, int> charToPiece = {
    {'P', P},
    {'N', N},
    {'B', B},
    {'R', R},
    {'Q', Q},
    {'K', K},
    {'p', p},
    {'n', n},
    {'b', b},
    {'r', r},
    {'q', q},
    {'k', k},
};

class Bitboard {
private:
	uint64_t bits;
	
public:
	Bitboard() { bits = 0ULL; }
	
	int getBit(int bit) { return (bits >> bit) & 1ULL; }
	void setBit(int bit) { bits |= 1ULL << bit; }
	void popBit(int bit) { bits &= ~(1ULL << bit); }
	
	uint64_t getBits() { return bits; }
	void setBits(uint64_t value) { bits = value; }
	
	void ORBits(uint64_t value) { bits |= value; }
	void ANDBits(uint64_t value) { bits &= value; }
	
	inline int countBits()
	{
		int count = 0;
		
		uint64_t bitsSave = bits;
		
		while (bits)
		{
			count++;
			bits &= bits - 1;
		}
		
		bits = bitsSave;
		
		return count;
	}
	
	inline int getLSB()
	{
		int index = 0;

		uint64_t bitsSave = bits;
		
		while ((bits & 1) == 0)
		{
			index++;
			bits >>= 1;
		}
		
		bits = bitsSave;

		return index;
	}
	
	void print()
	{
		for (int r = 0; r < 8; r++)
		{
			printf("%i ", 8 - r);
		
			for (int f = 0; f < 8; f++)
				printf("%i ", getBit(r * 8 + f));
			
			printf("\n");
		}
		
		printf("  a b c d e f g h\n\n");
	}
};

class Board {
private:
    Bitboard bitboards[12];
    Bitboard occupancy[3];
    
    int side;
    int castling;
    int enpassant;
	int ply;
    
public:
	Board()
	{
		side = WHITE;
		castling = 0;
		enpassant = noSq;
		ply = 0;

		memset(bitboards, 0, sizeof(bitboards));
		memset(occupancy, 0, sizeof(occupancy));
	}
	
	void print()
	{	
		for (int sq = 0; sq < 64; sq++)
		{
			bool noPiece = true;
		
			if (sq % 8 == 0)
				cout << endl << 8 - (sq / 8) << " ";
			
			for (int piece = P; piece <= k; piece++)
			{
				if (bitboards[piece].getBit(sq))
				{
					cout << asciiPieces[piece] << " ";
					noPiece = false;
					break;
				}
			}
			
			if (noPiece)
				cout << ". ";
		}
		
		cout << endl << "  a b c d e f g h" << endl << endl;
	}

	void parseFen(char* fen)
	{
		side = 0;
		enpassant = noSq;
		castling = 0;
		ply = 0;

		memset(bitboards, 0, sizeof(bitboards));
		memset(occupancy, 0, sizeof(occupancy));

		for (int rank = 0; rank < 8; rank++)
		{
			for (int file = 0; file < 8; file++)
			{
				int square = rank * 8 + file;

				if ((*fen >= 'a' && *fen <= 'z') || (*fen >= 'A' && *fen <= 'Z'))
				{
					int piece = charToPiece[*fen];
					bitboards[piece].setBit(square);
					fen++;
				}

				if (*fen >= '0' && *fen <= '9')
				{
					int offset = *fen - '0';
					int target_piece = -1;

					for (int piece = P; piece <= k; piece++)
						if (bitboards[piece].getBit(square))
							target_piece = piece;

					if (target_piece == -1)
						file--;

					file += offset;

					fen++;
				}

				if (*fen == '/')
					*fen++;
			}
		}

		// go to side to move
		fen++;

		(*fen == 'w') ? (side = WHITE) : (side = BLACK);

		// go to castling rights
		fen += 2;

		while (*fen != ' ')
		{
			switch (*fen)
			{
			case 'K': castling |= WK; break;
			case 'Q': castling |= WQ; break;
			case 'k': castling |= BK; break;
			case 'q': castling |= BQ; break;
			case '-': break;
			}

			fen++;
		}

		// go to enpassant square
		fen++;

		if (*fen != '-')
		{
			int file = fen[0] - 'a';
			int rank = 8 - (fen[1] - '0');

			enpassant = rank * 8 + file;
		}
		else
			enpassant = noSq;

		memset(occupancy, 0, sizeof(occupancy));

		for (int piece = P; piece <= K; piece++)
			occupancy[WHITE].ORBits(bitboards[piece].getBits());

		for (int piece = p; piece <= k; piece++)
			occupancy[BLACK].ORBits(bitboards[piece].getBits());

		occupancy[BOTH].setBits(occupancy[WHITE].getBits() | occupancy[BLACK].getBits());

		//hashKey = generate_hash_key();
	}

    Bitboard* getBitboard(int piece) { return &bitboards[piece]; }
    Bitboard* getOccupancy(int side) { return &occupancy[side]; }
    
    int getSide() { return side; }
	int getCastling() { return castling; }
	int getPly() { return ply; }
	int getEnpassant() { return enpassant; }
};

class MoveList {
private:
	int moves[256];
	int count;
	
public:
	inline int getMove(int index) { return moves[index]; }
	inline int getCount() { return count; }
	
	inline void addMove(int move) { moves[count++] = move; }
};

class MoveGenerator {
private:
	const uint64_t NOT_A_FILE = 18374403900871474942ULL;
	const uint64_t NOT_H_FILE = 9187201950435737471ULL;
	const uint64_t NOT_GH_FILE = 4557430888798830399ULL;
	const uint64_t NOT_AB_FILE = 18229723555195321596ULL;
	
	const int bishopRelevantBits[64] = {
        6, 5, 5, 5, 5, 5, 5, 6,
        5, 5, 5, 5, 5, 5, 5, 5,
        5, 5, 7, 7, 7, 7, 5, 5,
        5, 5, 7, 9, 9, 7, 5, 5,
        5, 5, 7, 9, 9, 7, 5, 5,
        5, 5, 7, 7, 7, 7, 5, 5,
        5, 5, 5, 5, 5, 5, 5, 5,
        6, 5, 5, 5, 5, 5, 5, 6
    };

    const int rookRelevantBits[64] = {
        12, 11, 11, 11, 11, 11, 11, 12,
        11, 10, 10, 10, 10, 10, 10, 11,
        11, 10, 10, 10, 10, 10, 10, 11,
        11, 10, 10, 10, 10, 10, 10, 11,
        11, 10, 10, 10, 10, 10, 10, 11,
        11, 10, 10, 10, 10, 10, 10, 11,
        11, 10, 10, 10, 10, 10, 10, 11,
        12, 11, 11, 11, 11, 11, 11, 12
    };

    uint64_t rookMagicNumbers[64] = {
        0x8a80104000800020ULL,
        0x140002000100040ULL,
        0x2801880a0017001ULL,
        0x100081001000420ULL,
        0x200020010080420ULL,
        0x3001c0002010008ULL,
        0x8480008002000100ULL,
        0x2080088004402900ULL,
        0x800098204000ULL,
        0x2024401000200040ULL,
        0x100802000801000ULL,
        0x120800800801000ULL,
        0x208808088000400ULL,
        0x2802200800400ULL,
        0x2200800100020080ULL,
        0x801000060821100ULL,
        0x80044006422000ULL,
        0x100808020004000ULL,
        0x12108a0010204200ULL,
        0x140848010000802ULL,
        0x481828014002800ULL,
        0x8094004002004100ULL,
        0x4010040010010802ULL,
        0x20008806104ULL,
        0x100400080208000ULL,
        0x2040002120081000ULL,
        0x21200680100081ULL,
        0x20100080080080ULL,
        0x2000a00200410ULL,
        0x20080800400ULL,
        0x80088400100102ULL,
        0x80004600042881ULL,
        0x4040008040800020ULL,
        0x440003000200801ULL,
        0x4200011004500ULL,
        0x188020010100100ULL,
        0x14800401802800ULL,
        0x2080040080800200ULL,
        0x124080204001001ULL,
        0x200046502000484ULL,
        0x480400080088020ULL,
        0x1000422010034000ULL,
        0x30200100110040ULL,
        0x100021010009ULL,
        0x2002080100110004ULL,
        0x202008004008002ULL,
        0x20020004010100ULL,
        0x2048440040820001ULL,
        0x101002200408200ULL,
        0x40802000401080ULL,
        0x4008142004410100ULL,
        0x2060820c0120200ULL,
        0x1001004080100ULL,
        0x20c020080040080ULL,
        0x2935610830022400ULL,
        0x44440041009200ULL,
        0x280001040802101ULL,
        0x2100190040002085ULL,
        0x80c0084100102001ULL,
        0x4024081001000421ULL,
        0x20030a0244872ULL,
        0x12001008414402ULL,
        0x2006104900a0804ULL,
        0x1004081002402ULL
    };

    uint64_t bishopMagicNumbers[64] = {
        0x40040844404084ULL,
        0x2004208a004208ULL,
        0x10190041080202ULL,
        0x108060845042010ULL,
        0x581104180800210ULL,
        0x2112080446200010ULL,
        0x1080820820060210ULL,
        0x3c0808410220200ULL,
        0x4050404440404ULL,
        0x21001420088ULL,
        0x24d0080801082102ULL,
        0x1020a0a020400ULL,
        0x40308200402ULL,
        0x4011002100800ULL,
        0x401484104104005ULL,
        0x801010402020200ULL,
        0x400210c3880100ULL,
        0x404022024108200ULL,
        0x810018200204102ULL,
        0x4002801a02003ULL,
        0x85040820080400ULL,
        0x810102c808880400ULL,
        0xe900410884800ULL,
        0x8002020480840102ULL,
        0x220200865090201ULL,
        0x2010100a02021202ULL,
        0x152048408022401ULL,
        0x20080002081110ULL,
        0x4001001021004000ULL,
        0x800040400a011002ULL,
        0xe4004081011002ULL,
        0x1c004001012080ULL,
        0x8004200962a00220ULL,
        0x8422100208500202ULL,
        0x2000402200300c08ULL,
        0x8646020080080080ULL,
        0x80020a0200100808ULL,
        0x2010004880111000ULL,
        0x623000a080011400ULL,
        0x42008c0340209202ULL,
        0x209188240001000ULL,
        0x400408a884001800ULL,
        0x110400a6080400ULL,
        0x1840060a44020800ULL,
        0x90080104000041ULL,
        0x201011000808101ULL,
        0x1a2208080504f080ULL,
        0x8012020600211212ULL,
        0x500861011240000ULL,
        0x180806108200800ULL,
        0x4000020e01040044ULL,
        0x300000261044000aULL,
        0x802241102020002ULL,
        0x20906061210001ULL,
        0x5a84841004010310ULL,
        0x4010801011c04ULL,
        0xa010109502200ULL,
        0x4a02012000ULL,
        0x500201010098b028ULL,
        0x8040002811040900ULL,
        0x28000010020204ULL,
        0x6000020202d0240ULL,
        0x8918844842082200ULL,
        0x4010011029020020ULL
    };
	
	Bitboard pawnAttacks[2][64];
	Bitboard knightAttacks[64];
	Bitboard kingAttacks[64];
	Bitboard bishopMasks[64];
	Bitboard rookMasks[64];
	Bitboard bishopAttacks[64][512];
	Bitboard rookAttacks[64][4096];
	
public:
	MoveGenerator()
	{
		for (int side = 0; side < 2; side++)
			for (int sq = 0; sq < 64; sq++)
				pawnAttacks[side][sq] = maskPawnAttacks(side, sq);
				
		for (int sq = 0; sq < 64; sq++)
		{
			knightAttacks[sq] = maskKnightAttacks(sq);
			kingAttacks[sq] = maskKingAttacks(sq);
			bishopMasks[sq] = maskBishopAttacks(sq);
			rookMasks[sq] = maskRookAttacks(sq);
		}
		
		initSliderAttacks(true);
		initSliderAttacks(false);
	}

	inline Bitboard getPawnAttacks(int side, int sq) { return pawnAttacks[side][sq]; }
	inline Bitboard getKnightAttacks(int sq) { return knightAttacks[sq]; }
	inline Bitboard getKingAttacks(int sq) { return kingAttacks[sq]; }
	
	inline Bitboard getRookAttacks(int sqr, Bitboard occupancy)
    {
        occupancy.ANDBits(rookMasks[sqr].getBits());
        occupancy.setBits(occupancy.getBits() * rookMagicNumbers[sqr]);
        occupancy.setBits(occupancy.getBits() >> 64 - rookRelevantBits[sqr]);

        return rookAttacks[sqr][occupancy.getBits()];
    }

    inline Bitboard getBishopAttacks(int sqr, Bitboard occupancy)
    {
        occupancy.ANDBits(bishopMasks[sqr].getBits());
        occupancy.setBits(occupancy.getBits() * bishopMagicNumbers[sqr]);
        occupancy.setBits(occupancy.getBits() >> 64 - bishopRelevantBits[sqr]);

        return bishopAttacks[sqr][occupancy.getBits()];
    }

	inline void generateMoves(Board board, MoveList* moves)
	{
		Bitboard* bitboard;
		
		int sq = 0, tSq = 0;
	
		for (int piece = P; piece <= k; piece++)
		{
			if (board.getSide() == WHITE)
			{
				if (piece == P)
				{
					bitboard = board.getBitboard(P);
					
					while (bitboard->getBits())
					{
						sq = bitboard->getLSB();
						tSq = sq - 8;
						
						if (!board.getOccupancy(BOTH)->getBit(tSq))
						{
							if (sq >= a7 && sq <= h7)
							{
								cout << "Pawn Promotion" << endl;
							}
							else
							{
								cout << "Pawn move: " << notation[sq] << notation[tSq] << endl;
								
								if (!board.getOccupancy(BOTH)->getBit(tSq - 8))
									cout << "Double pawn move: " << notation[sq] << notation[tSq - 8] << endl;
							}
						}
												
						bitboard->popBit(sq);
					}
				}
			}
		}
	}
	
	void initSliderAttacks(bool isBishop)
    {
        for (int sqr = 0; sqr < 64; sqr++)
        {
            Bitboard attackMask = isBishop ? bishopMasks[sqr] : rookMasks[sqr];
            int relevantBitsCount = attackMask.countBits();
            int occupancyIndices = (1 << relevantBitsCount);

            for (int index = 0; index < occupancyIndices; index++)
            {
                if (isBishop)
                {
                    Bitboard occupancy = setOccupancy(index, relevantBitsCount, attackMask);
                    int magic_index = (occupancy.getBits() * bishopMagicNumbers[sqr]) >> (64 - bishopRelevantBits[sqr]);
                    bishopAttacks[sqr][magic_index] = bishopAttacksOTF(sqr, occupancy);
                }
                else
                {
                    Bitboard occupancy = setOccupancy(index, relevantBitsCount, attackMask);
                    int magic_index = (occupancy.getBits() * rookMagicNumbers[sqr]) >> (64 - rookRelevantBits[sqr]);
                    rookAttacks[sqr][magic_index] = rookAttacksOTF(sqr, occupancy);
                }
            }
	    }
    }
	
	Bitboard setOccupancy(int index, int bitsInMask, Bitboard mask)
	{
		Bitboard occ;
		
		for (int i = 0; i < bitsInMask; i++)
		{
			int sq = mask.getLSB();
			
			mask.popBit(sq);
			
			if (index & (1 << i))
				occ.ORBits(1ULL << sq);
		}
		
		return occ;
	}

	Bitboard maskPawnAttacks(int side, int sq)
	{
		Bitboard attacks, bitboard;
		
		bitboard.setBit(sq);
		
		if (side == WHITE)
		{
			attacks.ORBits((bitboard.getBits() >> 7) & NOT_A_FILE);
			attacks.ORBits((bitboard.getBits() >> 9) & NOT_H_FILE);
		}
		else
		{
			attacks.ORBits((bitboard.getBits() << 7) & NOT_H_FILE);
			attacks.ORBits((bitboard.getBits() << 9) & NOT_A_FILE);
		}
		
		return attacks;
	}
	
	Bitboard maskKnightAttacks(int sq)
	{
		Bitboard attacks, bitboard;
		
		bitboard.setBit(sq);
		
		attacks.ORBits((bitboard.getBits() << 6) & NOT_GH_FILE);
		attacks.ORBits((bitboard.getBits() << 10) & NOT_AB_FILE);
		attacks.ORBits((bitboard.getBits() << 15) & NOT_H_FILE);
		attacks.ORBits((bitboard.getBits() << 17) & NOT_A_FILE);
		attacks.ORBits((bitboard.getBits() >> 6) & NOT_AB_FILE);
		attacks.ORBits((bitboard.getBits() >> 10) & NOT_GH_FILE);
		attacks.ORBits((bitboard.getBits() >> 15) & NOT_A_FILE);
		attacks.ORBits((bitboard.getBits() >> 17) & NOT_H_FILE);
		
		return attacks;
	}
	
	Bitboard maskKingAttacks(int sq)
	{
		Bitboard bb, attacks;
		
		bb.setBit(sq);
		
		attacks.ORBits((bb.getBits() << 1) & NOT_A_FILE);
		attacks.ORBits((bb.getBits() >> 1) & NOT_H_FILE);
		attacks.ORBits((bb.getBits() >> 7) & NOT_A_FILE);
		attacks.ORBits((bb.getBits() << 7) & NOT_H_FILE);
		attacks.ORBits(bb.getBits() >> 8);
		attacks.ORBits(bb.getBits() << 8);
		attacks.ORBits((bb.getBits() >> 9) & NOT_H_FILE);
		attacks.ORBits((bb.getBits() << 9) & NOT_A_FILE);
		
		return attacks;
	}
	
	Bitboard maskBishopAttacks(int sq)
	{
		Bitboard attacks;

        int r, f;

        int tr = sq / 8;
        int tf = sq % 8;

        for (r = tr + 1, f = tf + 1; r <= 6 && f <= 6; r++, f++) attacks.ORBits(1ULL << (r * 8 + f));
        for (r = tr - 1, f = tf + 1; r >= 1 && f <= 6; r--, f++) attacks.ORBits(1ULL << (r * 8 + f));
        for (r = tr + 1, f = tf - 1; r <= 6 && f >= 1; r++, f--) attacks.ORBits(1ULL << (r * 8 + f));
        for (r = tr - 1, f = tf - 1; r >= 1 && f >= 1; r--, f--) attacks.ORBits(1ULL << (r * 8 + f));

        return attacks;
	}
	
	Bitboard maskRookAttacks(int sq)
	{
		Bitboard attacks;
		
        int r, f;

        int tr = sq / 8;
        int tf = sq % 8;

        for (r = tr + 1; r <= 6; r++) attacks.ORBits(1ULL << (r * 8 + tf));
        for (r = tr - 1; r >= 1; r--) attacks.ORBits(1ULL << (r * 8 + tf));
        for (f = tf + 1; f <= 6; f++) attacks.ORBits(1ULL << (tr * 8 + f));
        for (f = tf - 1; f >= 1; f--) attacks.ORBits(1ULL << (tr * 8 + f));

        return attacks;
	}
	
	Bitboard bishopAttacksOTF(int sq, Bitboard block)
	{
		Bitboard attacks;

        int r, f;

        int tr = sq / 8;
        int tf = sq % 8;

        for (r = tr + 1, f = tf + 1; r <= 7 && f <= 7; r++, f++)
        {
            attacks.ORBits(1ULL << (r * 8 + f));
            if ((1ULL << (r * 8 + f)) & block.getBits()) break;
        }
        for (r = tr - 1, f = tf + 1; r >= 0 && f <= 7; r--, f++)
        {
            attacks.ORBits(1ULL << (r * 8 + f));
            if ((1ULL << (r * 8 + f)) & block.getBits()) break;
        }
        for (r = tr + 1, f = tf - 1; r <= 7 && f >= 0; r++, f--)
        {
            attacks.ORBits(1ULL << (r * 8 + f));
            if ((1ULL << (r * 8 + f)) & block.getBits()) break;
        }
        for (r = tr - 1, f = tf - 1; r >= 0 && f >= 0; r--, f--)
        {
            attacks.ORBits(1ULL << (r * 8 + f));
            if ((1ULL << (r * 8 + f)) & block.getBits()) break;
        }

        return attacks;
	}
	
	Bitboard rookAttacksOTF(int sq, Bitboard block)
	{
		Bitboard attacks;

        int r, f;

        int tr = sq / 8;
        int tf = sq % 8;

        for (r = tr + 1; r <= 7; r++)
        {
            attacks.ORBits(1ULL << (r * 8 + tf));
            if ((1ULL << (r * 8 + tf)) & block.getBits()) break;
        }

        for (r = tr - 1; r >= 0; r--)
        {
            attacks.ORBits(1ULL << (r * 8 + tf));
            if ((1ULL << (r * 8 + tf)) & block.getBits()) break;
        }

        for (f = tf + 1; f <= 7; f++)
        {
            attacks.ORBits(1ULL << (tr * 8 + f));
            if ((1ULL << (tr * 8 + f)) & block.getBits()) break;
        }

        for (f = tf - 1; f >= 0; f--)
        {
            attacks.ORBits(1ULL << (tr * 8 + f));
            if ((1ULL << (tr * 8 + f)) & block.getBits()) break;
        }

        return attacks;
	}
};

int main()
{
	Board board;
	MoveGenerator movegen;

	MoveList moves[1];

	board.parseFen(startPosition);
	board.print();
	movegen.generateMoves(board, moves);
	
	for (int i = 0; i < 64; i++)
		movegen.getBishopAttacks(i, *board.getOccupancy(BOTH)).print();

	return 0;
}
