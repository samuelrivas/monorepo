package sets

type Set map[string]struct{}

func Member(x string, s Set) bool {
	_, ok := s[x]
	return ok
}

func Make() Set {
	return make(map[string]struct{})
}

func Add(x string, s Set) {
	s[x] = struct{}{}
}
