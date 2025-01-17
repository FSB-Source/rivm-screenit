/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2025 Facilitaire Samenwerking Bevolkingsonderzoek
 * %%
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 * 
 * You should have received a copy of the GNU Affero General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 * =========================LICENSE_END==================================
 */

(function()
{
	var doc = document;
	var f = window.PIE;
	if (!f)
	{
		f = window.PIE = {
			F: "-pie-",
			nb: "Pie",
			La: "pie_",
			Ac: {
				TD: 1,
				TH: 1
			},
			cc: {
				TABLE: 1,
				THEAD: 1,
				TBODY: 1,
				TFOOT: 1,
				TR: 1,
				INPUT: 1,
				TEXTAREA: 1,
				SELECT: 1,
				OPTION: 1,
				IMG: 1,
				HR: 1
			},
			fc: {
				A: 1,
				INPUT: 1,
				TEXTAREA: 1,
				SELECT: 1,
				BUTTON: 1
			},
			Gd: {
				submit: 1,
				button: 1,
				reset: 1
			},
			aa: function()
			{
			}
		};
		try
		{
			doc.execCommand("BackgroundImageCache", false, true)
		}
		catch (aa)
		{
		}
		for (var ba = 4, Z = doc.createElement("div"), ca = Z.getElementsByTagName("i"), ga; Z.innerHTML = "<!--[if gt IE " + ++ba + "]><i></i><![endif]--\>", ca[0];)
			;
		f.O = ba;
		if (ba === 6)
			f.F = f.F.replace(/^-/, "");
		f.ja = doc.documentMode || f.O;
		Z.innerHTML = '<v:shape adj="1"/>';
		ga = Z.firstChild;
		ga.style.behavior = "url(#default#VML)";
		f.zc = typeof ga.adj === "object";
		(function()
		{
			var a, b = 0, c = {};
			f.p = {
				Za: function(d)
				{
					if (!a)
					{
						a = doc.createDocumentFragment();
						a.namespaces.add("css3vml", "urn:schemas-microsoft-com:vml")
					}
					return a.createElement("css3vml:" + d)
				},
				Ba: function(d)
				{
					return d && d._pieId || (d._pieId = "_" + ++b)
				},
				Eb: function(d)
				{
					var e, g, j, i, h = arguments;
					e = 1;
					for (g = h.length; e < g; e++)
					{
						i = h[e];
						for (j in i)
							if (i.hasOwnProperty(j))
								d[j] = i[j]
					}
					return d
				},
				Rb: function(d, e, g)
				{
					var j = c[d], i, h;
					if (j)
						Object.prototype.toString.call(j) === "[object Array]" ? j.push([e, g]) : e.call(g, j);
					else
					{
						h = c[d] = [[e, g]];
						i = new Image;
						i.onload = function()
						{
							j = c[d] = {
								h: i.width,
								f: i.height
							};
							for (var k = 0, n = h.length; k < n; k++)
								h[k][0].call(h[k][1], j);
							i.onload = null
						};
						i.src = d
					}
				}
			}
		})();
		f.Na = {
			gc: function(a, b, c, d)
			{
				function e()
				{
					k = j >= 90 && j < 270 ? b : 0;
					n = j < 180 ? c : 0;
					m = b - k;
					p = c - n
				}
				function g()
				{
					for (; j < 0;)
						j += 360;
					j %= 360
				}
				var j = d.sa;
				d = d.zb;
				var i, h, k, n, m, p, r, t;
				if (d)
				{
					d = d.coords(a, b, c);
					i = d.x;
					h = d.y
				}
				if (j)
				{
					j = j.jd();
					g();
					e();
					if (!d)
					{
						i = k;
						h = n
					}
					d = f.Na.tc(i, h, j, m, p);
					a = d[0];
					d = d[1]
				}
				else if (d)
				{
					a = b - i;
					d = c - h
				}
				else
				{
					i = h = a = 0;
					d = c
				}
				r = a - i;
				t = d - h;
				if (j === void 0)
				{
					j = !r ? t < 0 ? 90 : 270 : !t ? r < 0 ? 180 : 0 : -Math.atan2(t, r) / Math.PI * 180;
					g();
					e()
				}
				return {
					sa: j,
					xc: i,
					yc: h,
					td: a,
					ud: d,
					Wd: k,
					Xd: n,
					rd: m,
					sd: p,
					kd: r,
					ld: t,
					rc: f.Na.dc(i, h, a, d)
				}
			},
			tc: function(a, b, c, d, e)
			{
				if (c === 0 || c === 180)
					return [d, b];
				else if (c === 90 || c === 270)
					return [a, e];
				else
				{
					c = Math.tan(-c * Math.PI / 180);
					a = c * a - b;
					b = -1 / c;
					d = b * d - e;
					e = b - c;
					return [(d - a) / e, (c * d - b * a) / e]
				}
			},
			dc: function(a, b, c, d)
			{
				a = c - a;
				b = d - b;
				return Math.abs(a === 0 ? b : b === 0 ? a : Math.sqrt(a * a + b * b))
			}
		};
		f.ea = function()
		{
			this.Gb = [];
			this.oc = {}
		};
		f.ea.prototype = {
			ba: function(a)
			{
				var b = f.p.Ba(a), c = this.oc, d = this.Gb;
				if (!(b in c))
				{
					c[b] = d.length;
					d.push(a)
				}
			},
			Ha: function(a)
			{
				a = f.p.Ba(a);
				var b = this.oc;
				if (a && a in b)
				{
					delete this.Gb[b[a]];
					delete b[a]
				}
			},
			xa: function()
			{
				for (var a = this.Gb, b = a.length; b--;)
					a[b] && a[b]()
			}
		};
		f.Oa = new f.ea;
		f.Oa.Rd = function()
		{
			var a = this, b;
			if (!a.Sd)
			{
				b = doc.documentElement.currentStyle.getAttribute(f.F + "poll-interval") || 250;
				(function c()
				{
					a.xa();
					setTimeout(c, b)
				})();
				a.Sd = 1
			}
		};
		(function()
		{
			function a()
			{
				f.L.xa();
				window.detachEvent("onunload", a);
				window.PIE = null
			}
			f.L = new f.ea;
			window.attachEvent("onunload", a);
			f.L.ta = function(b, c, d)
			{
				b.attachEvent(c, d);
				this.ba(function()
				{
					b.detachEvent(c, d)
				})
			}
		})();
		f.Qa = new f.ea;
		f.L.ta(window, "onresize", function()
		{
			f.Qa.xa()
		});
		(function()
		{
			function a()
			{
				f.mb.xa()
			}
			f.mb = new f.ea;
			f.L.ta(window, "onscroll", a);
			f.Qa.ba(a)
		})();
		(function()
		{
			function a()
			{
				c = f.kb.md()
			}
			function b()
			{
				if (c)
				{
					for (var d = 0, e = c.length; d < e; d++)
						f.attach(c[d]);
					c = 0
				}
			}
			var c;
			if (f.ja < 9)
			{
				f.L.ta(window, "onbeforeprint", a);
				f.L.ta(window, "onafterprint", b)
			}
		})();
		f.lb = new f.ea;
		f.L.ta(doc, "onmouseup", function()
		{
			f.lb.xa()
		});
		f.he = function()
		{
			function a(h)
			{
				this.Y = h
			}
			var b = doc.createElement("length-calc"), c = doc.body || doc.documentElement, d = b.style, e = {}, g = ["mm", "cm", "in", "pt", "pc"], j = g.length, i = {};
			d.position = "absolute";
			d.top = d.left = "-9999px";
			for (c.appendChild(b); j--;)
			{
				d.width = "100" + g[j];
				e[g[j]] = b.offsetWidth / 100
			}
			c.removeChild(b);
			d.width = "1em";
			a.prototype = {
				Kb: /(px|em|ex|mm|cm|in|pt|pc|%)$/,
				ic: function()
				{
					var h = this.Jd;
					if (h === void 0)
						h = this.Jd = parseFloat(this.Y);
					return h
				},
				yb: function()
				{
					var h = this.ae;
					if (!h)
						h = this.ae = (h = this.Y.match(this.Kb)) && h[0] || "px";
					return h
				},
				a: function(h, k)
				{
					var n = this.ic(), m = this.yb();
					switch (m)
					{
					case "px":
						return n;
					case "%":
						return n * (typeof k === "function" ? k() : k) / 100;
					case "em":
						return n * this.xb(h);
					case "ex":
						return n * this.xb(h) / 2;
					default:
						return n * e[m]
					}
				},
				xb: function(h)
				{
					var k = h.currentStyle.fontSize, n, m;
					if (k.indexOf("px") > 0)
						return parseFloat(k);
					else if (h.tagName in f.cc)
					{
						m = this;
						n = h.parentNode;
						return f.n(k).a(n, function()
						{
							return m.xb(n)
						})
					}
					else
					{
						h.appendChild(b);
						k = b.offsetWidth;
						b.parentNode === h && h.removeChild(b);
						return k
					}
				}
			};
			f.n = function(h)
			{
				return i[h] || (i[h] = new a(h))
			};
			return a
		}();
		f.Ja = function()
		{
			function a(e)
			{
				this.X = e
			}
			var b = f.n("50%"), c = {
				top: 1,
				center: 1,
				bottom: 1
			}, d = {
				left: 1,
				center: 1,
				right: 1
			};
			a.prototype = {
				zd: function()
				{
					if (!this.ac)
					{
						var e = this.X, g = e.length, j = f.v, i = j.qa, h = f.n("0");
						i = i.na;
						h = ["left", h, "top", h];
						if (g === 1)
						{
							e.push(new j.ob(i, "center"));
							g++
						}
						if (g === 2)
						{
							i & (e[0].k | e[1].k) && e[0].d in c && e[1].d in d && e.push(e.shift());
							if (e[0].k & i)
								if (e[0].d === "center")
									h[1] = b;
								else
									h[0] = e[0].d;
							else if (e[0].W())
								h[1] = f.n(e[0].d);
							if (e[1].k & i)
								if (e[1].d === "center")
									h[3] = b;
								else
									h[2] = e[1].d;
							else if (e[1].W())
								h[3] = f.n(e[1].d)
						}
						this.ac = h
					}
					return this.ac
				},
				coords: function(e, g, j)
				{
					var i = this.zd(), h = i[1].a(e, g);
					e = i[3].a(e, j);
					return {
						x: i[0] === "right" ? g - h : h,
						y: i[2] === "bottom" ? j - e : e
					}
				}
			};
			return a
		}();
		f.Ka = function()
		{
			function a(b, c)
			{
				this.h = b;
				this.f = c
			}
			a.prototype = {
				a: function(b, c, d, e, g)
				{
					var j = this.h, i = this.f, h = c / d;
					e = e / g;
					if (j === "contain")
					{
						j = e > h ? c : d * e;
						i = e > h ? c / e : d
					}
					else if (j === "cover")
					{
						j = e < h ? c : d * e;
						i = e < h ? c / e : d
					}
					else if (j === "auto")
					{
						i = i === "auto" ? g : i.a(b, d);
						j = i * e
					}
					else
					{
						j = j.a(b, c);
						i = i === "auto" ? j / e : i.a(b, d)
					}
					return {
						h: j,
						f: i
					}
				}
			};
			a.Kc = new a("auto", "auto");
			return a
		}();
		f.Ec = function()
		{
			function a(b)
			{
				this.Y = b
			}
			a.prototype = {
				Kb: /[a-z]+$/i,
				yb: function()
				{
					return this.ad || (this.ad = this.Y.match(this.Kb)[0].toLowerCase())
				},
				jd: function()
				{
					var b = this.Vc, c;
					if (b === undefined)
					{
						b = this.yb();
						c = parseFloat(this.Y, 10);
						b = this.Vc = b === "deg" ? c : b === "rad" ? c / Math.PI * 180 : b === "grad" ? c / 400 * 360 : b === "turn" ? c * 360 : 0
					}
					return b
				}
			};
			return a
		}();
		f.Jc = function()
		{
			function a(c)
			{
				this.Y = c
			}
			var b = {};
			a.Qd = /\s*rgba\(\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d{1,3})\s*,\s*(\d+|\d*\.\d+)\s*\)\s*/;
			a.Fb = {
				aliceblue: "F0F8FF",
				antiquewhite: "FAEBD7",
				aqua: "0FF",
				aquamarine: "7FFFD4",
				azure: "F0FFFF",
				beige: "F5F5DC",
				bisque: "FFE4C4",
				black: "000",
				blanchedalmond: "FFEBCD",
				blue: "00F",
				blueviolet: "8A2BE2",
				brown: "A52A2A",
				burlywood: "DEB887",
				cadetblue: "5F9EA0",
				chartreuse: "7FFF00",
				chocolate: "D2691E",
				coral: "FF7F50",
				cornflowerblue: "6495ED",
				cornsilk: "FFF8DC",
				crimson: "DC143C",
				cyan: "0FF",
				darkblue: "00008B",
				darkcyan: "008B8B",
				darkgoldenrod: "B8860B",
				darkgray: "A9A9A9",
				darkgreen: "006400",
				darkkhaki: "BDB76B",
				darkmagenta: "8B008B",
				darkolivegreen: "556B2F",
				darkorange: "FF8C00",
				darkorchid: "9932CC",
				darkred: "8B0000",
				darksalmon: "E9967A",
				darkseagreen: "8FBC8F",
				darkslateblue: "483D8B",
				darkslategray: "2F4F4F",
				darkturquoise: "00CED1",
				darkviolet: "9400D3",
				deeppink: "FF1493",
				deepskyblue: "00BFFF",
				dimgray: "696969",
				dodgerblue: "1E90FF",
				firebrick: "B22222",
				floralwhite: "FFFAF0",
				forestgreen: "228B22",
				fuchsia: "F0F",
				gainsboro: "DCDCDC",
				ghostwhite: "F8F8FF",
				gold: "FFD700",
				goldenrod: "DAA520",
				gray: "808080",
				green: "008000",
				greenyellow: "ADFF2F",
				honeydew: "F0FFF0",
				hotpink: "FF69B4",
				indianred: "CD5C5C",
				indigo: "4B0082",
				ivory: "FFFFF0",
				khaki: "F0E68C",
				lavender: "E6E6FA",
				lavenderblush: "FFF0F5",
				lawngreen: "7CFC00",
				lemonchiffon: "FFFACD",
				lightblue: "ADD8E6",
				lightcoral: "F08080",
				lightcyan: "E0FFFF",
				lightgoldenrodyellow: "FAFAD2",
				lightgreen: "90EE90",
				lightgrey: "D3D3D3",
				lightpink: "FFB6C1",
				lightsalmon: "FFA07A",
				lightseagreen: "20B2AA",
				lightskyblue: "87CEFA",
				lightslategray: "789",
				lightsteelblue: "B0C4DE",
				lightyellow: "FFFFE0",
				lime: "0F0",
				limegreen: "32CD32",
				linen: "FAF0E6",
				magenta: "F0F",
				maroon: "800000",
				mediumauqamarine: "66CDAA",
				mediumblue: "0000CD",
				mediumorchid: "BA55D3",
				mediumpurple: "9370D8",
				mediumseagreen: "3CB371",
				mediumslateblue: "7B68EE",
				mediumspringgreen: "00FA9A",
				mediumturquoise: "48D1CC",
				mediumvioletred: "C71585",
				midnightblue: "191970",
				mintcream: "F5FFFA",
				mistyrose: "FFE4E1",
				moccasin: "FFE4B5",
				navajowhite: "FFDEAD",
				navy: "000080",
				oldlace: "FDF5E6",
				olive: "808000",
				olivedrab: "688E23",
				orange: "FFA500",
				orangered: "FF4500",
				orchid: "DA70D6",
				palegoldenrod: "EEE8AA",
				palegreen: "98FB98",
				paleturquoise: "AFEEEE",
				palevioletred: "D87093",
				papayawhip: "FFEFD5",
				peachpuff: "FFDAB9",
				peru: "CD853F",
				pink: "FFC0CB",
				plum: "DDA0DD",
				powderblue: "B0E0E6",
				purple: "800080",
				red: "F00",
				rosybrown: "BC8F8F",
				royalblue: "4169E1",
				saddlebrown: "8B4513",
				salmon: "FA8072",
				sandybrown: "F4A460",
				seagreen: "2E8B57",
				seashell: "FFF5EE",
				sienna: "A0522D",
				silver: "C0C0C0",
				skyblue: "87CEEB",
				slateblue: "6A5ACD",
				slategray: "708090",
				snow: "FFFAFA",
				springgreen: "00FF7F",
				steelblue: "4682B4",
				tan: "D2B48C",
				teal: "008080",
				thistle: "D8BFD8",
				tomato: "FF6347",
				turquoise: "40E0D0",
				violet: "EE82EE",
				wheat: "F5DEB3",
				white: "FFF",
				whitesmoke: "F5F5F5",
				yellow: "FF0",
				yellowgreen: "9ACD32"
			};
			a.prototype = {
				parse: function()
				{
					if (!this.Ua)
					{
						var c = this.Y, d;
						if (d = c.match(a.Qd))
						{
							this.Ua = "rgb(" + d[1] + "," + d[2] + "," + d[3] + ")";
							this.Yb = parseFloat(d[4])
						}
						else
						{
							if ((d = c.toLowerCase()) in a.Fb)
								c = "#" + a.Fb[d];
							this.Ua = c;
							this.Yb = c === "transparent" ? 0 : 1
						}
					}
				},
				U: function(c)
				{
					this.parse();
					return this.Ua === "currentColor" ? c.currentStyle.color : this.Ua
				},
				fa: function()
				{
					this.parse();
					return this.Yb
				}
			};
			f.ha = function(c)
			{
				return b[c] || (b[c] = new a(c))
			};
			return a
		}();
		f.v = function()
		{
			function a(c)
			{
				this.$a = c;
				this.ch = 0;
				this.X = [];
				this.Ga = 0
			}
			var b = a.qa = {
				Ia: 1,
				Wb: 2,
				z: 4,
				Lc: 8,
				Xb: 16,
				na: 32,
				K: 64,
				oa: 128,
				pa: 256,
				Ra: 512,
				Tc: 1024,
				URL: 2048
			};
			a.ob = function(c, d)
			{
				this.k = c;
				this.d = d
			};
			a.ob.prototype = {
				Ca: function()
				{
					return this.k & b.K || this.k & b.oa && this.d === "0"
				},
				W: function()
				{
					return this.Ca() || this.k & b.Ra
				}
			};
			a.prototype = {
				de: /\s/,
				Kd: /^[\+\-]?(\d*\.)?\d+/,
				url: /^url\(\s*("([^"]*)"|'([^']*)'|([!#$%&*-~]*))\s*\)/i,
				nc: /^\-?[_a-z][\w-]*/i,
				Yd: /^("([^"]*)"|'([^']*)')/,
				Bd: /^#([\da-f]{6}|[\da-f]{3})/i,
				be: {
					px: b.K,
					em: b.K,
					ex: b.K,
					mm: b.K,
					cm: b.K,
					"in": b.K,
					pt: b.K,
					pc: b.K,
					deg: b.Ia,
					rad: b.Ia,
					grad: b.Ia
				},
				fd: {
					rgb: 1,
					rgba: 1,
					hsl: 1,
					hsla: 1
				},
				next: function(c)
				{
					function d(p, r)
					{
						p = new a.ob(p, r);
						if (!c)
						{
							k.X.push(p);
							k.Ga++
						}
						return p
					}
					function e()
					{
						k.Ga++;
						return null
					}
					var g, j, i, h, k = this;
					if (this.Ga < this.X.length)
						return this.X[this.Ga++];
					for (; this.de.test(this.$a.charAt(this.ch));)
						this.ch++;
					if (this.ch >= this.$a.length)
						return e();
					j = this.ch;
					g = this.$a.substring(this.ch);
					i = g.charAt(0);
					switch (i)
					{
					case "#":
						if (h = g.match(this.Bd))
						{
							this.ch += h[0].length;
							return d(b.z, h[0])
						}
						break;
					case '"':
					case "'":
						if (h = g.match(this.Yd))
						{
							this.ch += h[0].length;
							return d(b.Tc, h[2] || h[3] || "")
						}
						break;
					case "/":
					case ",":
						this.ch++;
						return d(b.pa, i);
					case "u":
						if (h = g.match(this.url))
						{
							this.ch += h[0].length;
							return d(b.URL, h[2] || h[3] || h[4] || "")
						}
					}
					if (h = g.match(this.Kd))
					{
						i = h[0];
						this.ch += i.length;
						if (g.charAt(i.length) === "%")
						{
							this.ch++;
							return d(b.Ra, i + "%")
						}
						if (h = g.substring(i.length).match(this.nc))
						{
							i += h[0];
							this.ch += h[0].length;
							return d(this.be[h[0].toLowerCase()] || b.Lc, i)
						}
						return d(b.oa, i)
					}
					if (h = g.match(this.nc))
					{
						i = h[0];
						this.ch += i.length;
						if (i.toLowerCase() in f.Jc.Fb || i === "currentColor" || i === "transparent")
							return d(b.z, i);
						if (g.charAt(i.length) === "(")
						{
							this.ch++;
							if (i.toLowerCase() in this.fd)
							{
								g = function(p)
								{
									return p && p.k & b.oa
								};
								h = function(p)
								{
									return p && p.k & (b.oa | b.Ra)
								};
								var n = function(p, r)
								{
									return p && p.d === r
								}, m = function()
								{
									return k.next(1)
								};
								if ((i.charAt(0) === "r" ? h(m()) : g(m())) && n(m(), ",") && h(m()) && n(m(), ",") && h(m())
									&& (i === "rgb" || i === "hsa" || n(m(), ",") && g(m())) && n(m(), ")"))
									return d(b.z, this.$a.substring(j, this.ch));
								return e()
							}
							return d(b.Xb, i)
						}
						return d(b.na, i)
					}
					this.ch++;
					return d(b.Wb, i)
				},
				D: function()
				{
					return this.X[this.Ga-- - 2]
				},
				all: function()
				{
					for (; this.next();)
						;
					return this.X
				},
				ma: function(c, d)
				{
					for (var e = [], g, j; g = this.next();)
					{
						if (c(g))
						{
							j = true;
							this.D();
							break
						}
						e.push(g)
					}
					return d && !j ? null : e
				}
			};
			return a
		}();
		var ha = function(a)
		{
			this.e = a
		};
		ha.prototype = {
			Z: 0,
			Od: function()
			{
				var a = this.qb, b;
				return !a || (b = this.o()) && (a.x !== b.x || a.y !== b.y)
			},
			Td: function()
			{
				var a = this.qb, b;
				return !a || (b = this.o()) && (a.h !== b.h || a.f !== b.f)
			},
			hc: function()
			{
				var a = this.e, b = a.getBoundingClientRect(), c = f.ja === 9, d = f.O === 7, e = b.right - b.left;
				return {
					x: b.left,
					y: b.top,
					h: c || d ? a.offsetWidth : e,
					f: c || d ? a.offsetHeight : b.bottom - b.top,
					Hd: d && e ? a.offsetWidth / e : 1
				}
			},
			o: function()
			{
				return this.Z ? this.Va || (this.Va = this.hc()) : this.hc()
			},
			Ad: function()
			{
				return !!this.qb
			},
			cb: function()
			{
				++this.Z
			},
			hb: function()
			{
				if (!--this.Z)
				{
					if (this.Va)
						this.qb = this.Va;
					this.Va = null
				}
			}
		};
		(function()
		{
			function a(b)
			{
				var c = f.p.Ba(b);
				return function()
				{
					if (this.Z)
					{
						var d = this.$b || (this.$b = {});
						return c in d ? d[c] : (d[c] = b.call(this))
					}
					else
						return b.call(this)
				}
			}
			f.B = {
				Z: 0,
				ka: function(b)
				{
					function c(d)
					{
						this.e = d;
						this.Zb = this.ia()
					}
					f.p.Eb(c.prototype, f.B, b);
					c.$c = {};
					return c
				},
				j: function()
				{
					var b = this.ia(), c = this.constructor.$c;
					return b ? b in c ? c[b] : (c[b] = this.la(b)) : null
				},
				ia: a(function()
				{
					var b = this.e, c = this.constructor, d = b.style;
					b = b.currentStyle;
					var e = this.wa, g = this.Fa, j = c.Yc || (c.Yc = f.F + e);
					c = c.Zc || (c.Zc = f.nb + g.charAt(0).toUpperCase() + g.substring(1));
					return d[c] || b.getAttribute(j) || d[g] || b.getAttribute(e)
				}),
				i: a(function()
				{
					return !!this.j()
				}),
				H: a(function()
				{
					var b = this.ia(), c = b !== this.Zb;
					this.Zb = b;
					return c
				}),
				va: a,
				cb: function()
				{
					++this.Z
				},
				hb: function()
				{
					--this.Z || delete this.$b
				}
			}
		})();
		f.Sb = f.B.ka({
			wa: f.F + "background",
			Fa: f.nb + "Background",
			cd: {
				scroll: 1,
				fixed: 1,
				local: 1
			},
			fb: {
				"repeat-x": 1,
				"repeat-y": 1,
				repeat: 1,
				"no-repeat": 1
			},
			sc: {
				"padding-box": 1,
				"border-box": 1,
				"content-box": 1
			},
			Pd: {
				top: 1,
				right: 1,
				bottom: 1,
				left: 1,
				center: 1
			},
			Ud: {
				contain: 1,
				cover: 1
			},
			eb: {
				Ma: "backgroundClip",
				z: "backgroundColor",
				da: "backgroundImage",
				Pa: "backgroundOrigin",
				S: "backgroundPosition",
				T: "backgroundRepeat",
				Sa: "backgroundSize"
			},
			la: function(a)
			{
				function b(s)
				{
					return s && s.W() || s.k & k && s.d in t
				}
				function c(s)
				{
					return s && (s.W() && f.n(s.d) || s.d === "auto" && "auto")
				}
				var d = this.e.currentStyle, e, g, j, i = f.v.qa, h = i.pa, k = i.na, n = i.z, m, p, r = 0, t = this.Pd, v, l, q = {
					M: []
				};
				if (this.wb())
				{
					e = new f.v(a);
					for (j = {}; g = e.next();)
					{
						m = g.k;
						p = g.d;
						if (!j.P && m & i.Xb && p === "linear-gradient")
						{
							v = {
								ca: [],
								P: p
							};
							for (l = {}; g = e.next();)
							{
								m = g.k;
								p = g.d;
								if (m & i.Wb && p === ")")
								{
									l.color && v.ca.push(l);
									v.ca.length > 1 && f.p.Eb(j, v);
									break
								}
								if (m & n)
								{
									if (v.sa || v.zb)
									{
										g = e.D();
										if (g.k !== h)
											break;
										e.next()
									}
									l = {
										color: f.ha(p)
									};
									g = e.next();
									if (g.W())
										l.db = f.n(g.d);
									else
										e.D()
								}
								else if (m & i.Ia && !v.sa && !l.color && !v.ca.length)
									v.sa = new f.Ec(g.d);
								else if (b(g) && !v.zb && !l.color && !v.ca.length)
								{
									e.D();
									v.zb = new f.Ja(e.ma(function(s)
									{
										return !b(s)
									}, false))
								}
								else if (m & h && p === ",")
								{
									if (l.color)
									{
										v.ca.push(l);
										l = {}
									}
								}
								else
									break
							}
						}
						else if (!j.P && m & i.URL)
						{
							j.Ab = p;
							j.P = "image"
						}
						else if (b(g) && !j.$)
						{
							e.D();
							j.$ = new f.Ja(e.ma(function(s)
							{
								return !b(s)
							}, false))
						}
						else if (m & k)
							if (p in this.fb && !j.bb)
								j.bb = p;
							else if (p in this.sc && !j.Wa)
							{
								j.Wa = p;
								if ((g = e.next()) && g.k & k && g.d in this.sc)
									j.ub = g.d;
								else
								{
									j.ub = p;
									e.D()
								}
							}
							else if (p in this.cd && !j.bc)
								j.bc = p;
							else
								return null;
						else if (m & n && !q.color)
							q.color = f.ha(p);
						else if (m & h && p === "/" && !j.Xa && j.$)
						{
							g = e.next();
							if (g.k & k && g.d in this.Ud)
								j.Xa = new f.Ka(g.d);
							else if (g = c(g))
							{
								m = c(e.next());
								if (!m)
								{
									m = g;
									e.D()
								}
								j.Xa = new f.Ka(g, m)
							}
							else
								return null
						}
						else if (m & h && p === "," && j.P)
						{
							j.Hb = a.substring(r, e.ch - 1);
							r = e.ch;
							q.M.push(j);
							j = {}
						}
						else
							return null
					}
					if (j.P)
					{
						j.Hb = a.substring(r);
						q.M.push(j)
					}
				}
				else
					this.Bc(f.ja < 9 ? function()
					{
						var s = this.eb, o = d[s.S + "X"], u = d[s.S + "Y"], x = d[s.da], y = d[s.z];
						if (y !== "transparent")
							q.color = f.ha(y);
						if (x !== "none")
							q.M = [{
								P: "image",
								Ab: (new f.v(x)).next().d,
								bb: d[s.T],
								$: new f.Ja((new f.v(o + " " + u)).all())
							}]
					} : function()
					{
						var s = this.eb, o = /\s*,\s*/, u = d[s.da].split(o), x = d[s.z], y, z, B, E, D, C;
						if (x !== "transparent")
							q.color = f.ha(x);
						if ((E = u.length) && u[0] !== "none")
						{
							x = d[s.T].split(o);
							y = d[s.S].split(o);
							z = d[s.Pa].split(o);
							B = d[s.Ma].split(o);
							s = d[s.Sa].split(o);
							q.M = [];
							for (o = 0; o < E; o++)
								if ((D = u[o]) && D !== "none")
								{
									C = s[o].split(" ");
									q.M.push({
										Hb: D + " " + x[o] + " " + y[o] + " / " + s[o] + " " + z[o] + " " + B[o],
										P: "image",
										Ab: (new f.v(D)).next().d,
										bb: x[o],
										$: new f.Ja((new f.v(y[o])).all()),
										Wa: z[o],
										ub: B[o],
										Xa: new f.Ka(C[0], C[1])
									})
								}
						}
					});
				return q.color || q.M[0] ? q : null
			},
			Bc: function(a)
			{
				var b = f.ja > 8, c = this.eb, d = this.e.runtimeStyle, e = d[c.da], g = d[c.z], j = d[c.T], i, h, k, n;
				if (e)
					d[c.da] = "";
				if (g)
					d[c.z] = "";
				if (j)
					d[c.T] = "";
				if (b)
				{
					i = d[c.Ma];
					h = d[c.Pa];
					n = d[c.S];
					k = d[c.Sa];
					if (i)
						d[c.Ma] = "";
					if (h)
						d[c.Pa] = "";
					if (n)
						d[c.S] = "";
					if (k)
						d[c.Sa] = ""
				}
				a = a.call(this);
				if (e)
					d[c.da] = e;
				if (g)
					d[c.z] = g;
				if (j)
					d[c.T] = j;
				if (b)
				{
					if (i)
						d[c.Ma] = i;
					if (h)
						d[c.Pa] = h;
					if (n)
						d[c.S] = n;
					if (k)
						d[c.Sa] = k
				}
				return a
			},
			ia: f.B.va(function()
			{
				return this.wb() || this.Bc(function()
				{
					var a = this.e.currentStyle, b = this.eb;
					return a[b.z] + " " + a[b.da] + " " + a[b.T] + " " + a[b.S + "X"] + " " + a[b.S + "Y"]
				})
			}),
			wb: f.B.va(function()
			{
				var a = this.e;
				return a.style[this.Fa] || a.currentStyle.getAttribute(this.wa)
			}),
			qc: function()
			{
				var a = 0;
				if (f.O < 7)
				{
					a = this.e;
					a = "" + (a.style[f.nb + "PngFix"] || a.currentStyle.getAttribute(f.F + "png-fix")) === "true"
				}
				return a
			},
			i: f.B.va(function()
			{
				return (this.wb() || this.qc()) && !!this.j()
			})
		});
		f.Vb = f.B.ka({
			wc: ["Top", "Right", "Bottom", "Left"],
			Id: {
				thin: "1px",
				medium: "3px",
				thick: "5px"
			},
			la: function()
			{
				var a = {}, b = {}, c = {}, d = false, e = true, g = true, j = true;
				this.Cc(function()
				{
					for (var i = this.e.currentStyle, h = 0, k, n, m, p, r, t, v; h < 4; h++)
					{
						m = this.wc[h];
						v = m.charAt(0).toLowerCase();
						k = b[v] = i["border" + m + "Style"];
						n = i["border" + m + "Color"];
						m = i["border" + m + "Width"];
						if (h > 0)
						{
							if (k !== p)
								g = false;
							if (n !== r)
								e = false;
							if (m !== t)
								j = false
						}
						p = k;
						r = n;
						t = m;
						c[v] = f.ha(n);
						m = a[v] = f.n(b[v] === "none" ? "0" : this.Id[m] || m);
						if (m.a(this.e) > 0)
							d = true
					}
				});
				return d ? {
					J: a,
					Zd: b,
					gd: c,
					ee: j,
					hd: e,
					$d: g
				} : null
			},
			ia: f.B.va(function()
			{
				var a = this.e, b = a.currentStyle, c;
				a.tagName in f.Ac && a.offsetParent.currentStyle.borderCollapse === "collapse" || this.Cc(function()
				{
					c = b.borderWidth + "|" + b.borderStyle + "|" + b.borderColor
				});
				return c
			}),
			Cc: function(a)
			{
				var b = this.e.runtimeStyle, c = b.borderWidth, d = b.borderColor;
				if (c)
					b.borderWidth = "";
				if (d)
					b.borderColor = "";
				a = a.call(this);
				if (c)
					b.borderWidth = c;
				if (d)
					b.borderColor = d;
				return a
			}
		});
		(function()
		{
			f.jb = f.B.ka({
				wa: "border-radius",
				Fa: "borderRadius",
				la: function(b)
				{
					var c = null, d, e, g, j, i = false;
					if (b)
					{
						e = new f.v(b);
						var h = function()
						{
							for (var k = [], n; (g = e.next()) && g.W();)
							{
								j = f.n(g.d);
								n = j.ic();
								if (n < 0)
									return null;
								if (n > 0)
									i = true;
								k.push(j)
							}
							return k.length > 0 && k.length < 5 ? {
								tl: k[0],
								tr: k[1] || k[0],
								br: k[2] || k[0],
								bl: k[3] || k[1] || k[0]
							} : null
						};
						if (b = h())
						{
							if (g)
							{
								if (g.k & f.v.qa.pa && g.d === "/")
									d = h()
							}
							else
								d = b;
							if (i && b && d)
								c = {
									x: b,
									y: d
								}
						}
					}
					return c
				}
			});
			var a = f.n("0");
			a = {
				tl: a,
				tr: a,
				br: a,
				bl: a
			};
			f.jb.Dc = {
				x: a,
				y: a
			}
		})();
		f.Ub = f.B.ka({
			wa: "border-image",
			Fa: "borderImage",
			fb: {
				stretch: 1,
				round: 1,
				repeat: 1,
				space: 1
			},
			la: function(a)
			{
				var b = null, c, d, e, g, j, i, h = 0, k = f.v.qa, n = k.na, m = k.oa, p = k.Ra;
				if (a)
				{
					c = new f.v(a);
					b = {};
					for (var r = function(l)
					{
						return l && l.k & k.pa && l.d === "/"
					}, t = function(l)
					{
						return l && l.k & n && l.d === "fill"
					}, v = function()
					{
						g = c.ma(function(l)
						{
							return !(l.k & (m | p))
						});
						if (t(c.next()) && !b.fill)
							b.fill = true;
						else
							c.D();
						if (r(c.next()))
						{
							h++;
							j = c.ma(function(l)
							{
								return !l.W() && !(l.k & n && l.d === "auto")
							});
							if (r(c.next()))
							{
								h++;
								i = c.ma(function(l)
								{
									return !l.Ca()
								})
							}
						}
						else
							c.D()
					}; a = c.next();)
					{
						d = a.k;
						e = a.d;
						if (d & (m | p) && !g)
						{
							c.D();
							v()
						}
						else if (t(a) && !b.fill)
						{
							b.fill = true;
							v()
						}
						else if (d & n && this.fb[e] && !b.repeat)
						{
							b.repeat = {
								f: e
							};
							if (a = c.next())
								if (a.k & n && this.fb[a.d])
									b.repeat.Ob = a.d;
								else
									c.D()
						}
						else if (d & k.URL && !b.src)
							b.src = e;
						else
							return null
					}
					if (!b.src || !g || g.length < 1 || g.length > 4 || j && j.length > 4 || h === 1 && j.length < 1 || i && i.length > 4 || h === 2 && i.length < 1)
						return null;
					if (!b.repeat)
						b.repeat = {
							f: "stretch"
						};
					if (!b.repeat.Ob)
						b.repeat.Ob = b.repeat.f;
					a = function(l, q)
					{
						return {
							t: q(l[0]),
							r: q(l[1] || l[0]),
							b: q(l[2] || l[0]),
							l: q(l[3] || l[1] || l[0])
						}
					};
					b.slice = a(g, function(l)
					{
						return f.n(l.k & m ? l.d + "px" : l.d)
					});
					if (j && j[0])
						b.J = a(j, function(l)
						{
							return l.W() ? f.n(l.d) : l.d
						});
					if (i && i[0])
						b.Da = a(i, function(l)
						{
							return l.Ca() ? f.n(l.d) : l.d
						})
				}
				return b
			}
		});
		f.Ic = f.B.ka({
			wa: "box-shadow",
			Fa: "boxShadow",
			la: function(a)
			{
				var b, c = f.n, d = f.v.qa, e;
				if (a)
				{
					e = new f.v(a);
					b = {
						Da: [],
						Bb: []
					};
					for (a = function()
					{
						for (var g, j, i, h, k, n; g = e.next();)
						{
							i = g.d;
							j = g.k;
							if (j & d.pa && i === ",")
								break;
							else if (g.Ca() && !k)
							{
								e.D();
								k = e.ma(function(m)
								{
									return !m.Ca()
								})
							}
							else if (j & d.z && !h)
								h = i;
							else if (j & d.na && i === "inset" && !n)
								n = true;
							else
								return false
						}
						g = k && k.length;
						if (g > 1 && g < 5)
						{
							(n ? b.Bb : b.Da).push({
								fe: c(k[0].d),
								ge: c(k[1].d),
								blur: c(k[2] ? k[2].d : "0"),
								Vd: c(k[3] ? k[3].d : "0"),
								color: f.ha(h || "currentColor")
							});
							return true
						}
						return false
					}; a();)
						;
				}
				return b && (b.Bb.length || b.Da.length) ? b : null
			}
		});
		f.Uc = f.B.ka({
			ia: f.B.va(function()
			{
				var a = this.e.currentStyle;
				return a.visibility + "|" + a.display
			}),
			la: function()
			{
				var a = this.e, b = a.runtimeStyle;
				a = a.currentStyle;
				var c = b.visibility, d;
				b.visibility = "";
				d = a.visibility;
				b.visibility = c;
				return {
					ce: d !== "hidden",
					nd: a.display !== "none"
				}
			},
			i: function()
			{
				return false
			}
		});
		f.u = {
			R: function(a)
			{
				function b(c, d, e, g)
				{
					this.e = c;
					this.s = d;
					this.g = e;
					this.parent = g
				}
				f.p.Eb(b.prototype, f.u, a);
				return b
			},
			Cb: false,
			Q: function()
			{
				return false
			},
			Ea: f.aa,
			Lb: function()
			{
				this.m();
				this.i() && this.V()
			},
			ib: function()
			{
				this.Cb = true
			},
			Mb: function()
			{
				this.i() ? this.V() : this.m()
			},
			sb: function(a, b)
			{
				this.vc(a);
				for (var c = this.ra || (this.ra = []), d = a + 1, e = c.length, g; d < e; d++)
					if (g = c[d])
						break;
				c[a] = b;
				this.I().insertBefore(b, g || null)
			},
			za: function(a)
			{
				var b = this.ra;
				return b && b[a] || null
			},
			vc: function(a)
			{
				var b = this.za(a), c = this.Ta;
				if (b && c)
				{
					c.removeChild(b);
					this.ra[a] = null
				}
			},
			Aa: function(a, b, c, d)
			{
				var e = this.rb || (this.rb = {}), g = e[a];
				if (!g)
				{
					g = e[a] = f.p.Za("shape");
					if (b)
						g.appendChild(g[b] = f.p.Za(b));
					if (d)
					{
						c = this.za(d);
						if (!c)
						{
							this.sb(d, doc.createElement("group" + d));
							c = this.za(d)
						}
					}
					c.appendChild(g);
					a = g.style;
					a.position = "absolute";
					a.left = a.top = 0;
					a.behavior = "url(#default#VML)"
				}
				return g
			},
			vb: function(a)
			{
				var b = this.rb, c = b && b[a];
				if (c)
				{
					c.parentNode.removeChild(c);
					delete b[a]
				}
				return !!c
			},
			kc: function(a)
			{
				var b = this.e, c = this.s.o(), d = c.h, e = c.f, g, j, i, h, k, n;
				c = a.x.tl.a(b, d);
				g = a.y.tl.a(b, e);
				j = a.x.tr.a(b, d);
				i = a.y.tr.a(b, e);
				h = a.x.br.a(b, d);
				k = a.y.br.a(b, e);
				n = a.x.bl.a(b, d);
				a = a.y.bl.a(b, e);
				d = Math.min(d / (c + j), e / (i + k), d / (n + h), e / (g + a));
				if (d < 1)
				{
					c *= d;
					g *= d;
					j *= d;
					i *= d;
					h *= d;
					k *= d;
					n *= d;
					a *= d
				}
				return {
					x: {
						tl: c,
						tr: j,
						br: h,
						bl: n
					},
					y: {
						tl: g,
						tr: i,
						br: k,
						bl: a
					}
				}
			},
			ya: function(a, b, c)
			{
				b = b || 1;
				var d, e, g = this.s.o();
				e = g.h * b;
				g = g.f * b;
				var j = this.g.G, i = Math.floor, h = Math.ceil, k = a ? a.Jb * b : 0, n = a ? a.Ib * b : 0, m = a ? a.tb * b : 0;
				a = a ? a.Db * b : 0;
				var p, r, t, v, l;
				if (c || j.i())
				{
					d = this.kc(c || j.j());
					c = d.x.tl * b;
					j = d.y.tl * b;
					p = d.x.tr * b;
					r = d.y.tr * b;
					t = d.x.br * b;
					v = d.y.br * b;
					l = d.x.bl * b;
					b = d.y.bl * b;
					e = "m" + i(a) + "," + i(j) + "qy" + i(c) + "," + i(k) + "l" + h(e - p) + "," + i(k) + "qx" + h(e - n) + "," + i(r) + "l" + h(e - n) + "," + h(g - v) + "qy"
						+ h(e - t) + "," + h(g - m) + "l" + i(l) + "," + h(g - m) + "qx" + i(a) + "," + h(g - b) + " x e"
				}
				else
					e = "m" + i(a) + "," + i(k) + "l" + h(e - n) + "," + i(k) + "l" + h(e - n) + "," + h(g - m) + "l" + i(a) + "," + h(g - m) + "xe";
				return e
			},
			I: function()
			{
				var a = this.parent.za(this.N), b;
				if (!a)
				{
					a = doc.createElement(this.Ya);
					b = a.style;
					b.position = "absolute";
					b.top = b.left = 0;
					this.parent.sb(this.N, a)
				}
				return a
			},
			mc: function()
			{
				var a = this.e, b = a.currentStyle, c = a.runtimeStyle, d = a.tagName, e = f.O === 6, g;
				if (e && (d in f.cc || d === "FIELDSET") || d === "BUTTON" || d === "INPUT" && a.type in f.Gd)
				{
					c.borderWidth = "";
					d = this.g.w.wc;
					for (g = d.length; g--;)
					{
						e = d[g];
						c["padding" + e] = "";
						c["padding" + e] = f.n(b["padding" + e]).a(a) + f.n(b["border" + e + "Width"]).a(a) + (f.O !== 8 && g % 2 ? 1 : 0)
					}
					c.borderWidth = 0
				}
				else if (e)
				{
					if (a.childNodes.length !== 1 || a.firstChild.tagName !== "ie6-mask")
					{
						b = doc.createElement("ie6-mask");
						d = b.style;
						d.visibility = "visible";
						for (d.zoom = 1; d = a.firstChild;)
							b.appendChild(d);
						a.appendChild(b);
						c.visibility = "hidden"
					}
				}
				else
					c.borderColor = "transparent"
			},
			ie: function()
			{
			},
			m: function()
			{
				this.parent.vc(this.N);
				delete this.rb;
				delete this.ra
			}
		};
		f.Rc = f.u.R({
			i: function()
			{
				var a = this.ed;
				for ( var b in a)
					if (a.hasOwnProperty(b) && a[b].i())
						return true;
				return false
			},
			Q: function()
			{
				return this.g.Pb.H()
			},
			ib: function()
			{
				if (this.i())
				{
					var a = this.jc(), b = a, c;
					a = a.currentStyle;
					var d = a.position, e = this.I().style, g = 0, j = 0;
					j = this.s.o();
					var i = j.Hd;
					if (d === "fixed" && f.O > 6)
					{
						g = j.x * i;
						j = j.y * i;
						b = d
					}
					else
					{
						do
							b = b.offsetParent;
						while (b && b.currentStyle.position === "static");
						if (b)
						{
							c = b.getBoundingClientRect();
							b = b.currentStyle;
							g = (j.x - c.left) * i - (parseFloat(b.borderLeftWidth) || 0);
							j = (j.y - c.top) * i - (parseFloat(b.borderTopWidth) || 0)
						}
						else
						{
							b = doc.documentElement;
							g = (j.x + b.scrollLeft - b.clientLeft) * i;
							j = (j.y + b.scrollTop - b.clientTop) * i
						}
						b = "absolute"
					}
					e.position = b;
					e.left = g;
					e.top = j;
					e.zIndex = d === "static" ? -1 : a.zIndex;
					this.Cb = true
				}
			},
			Mb: f.aa,
			Nb: function()
			{
				var a = this.g.Pb.j();
				this.I().style.display = a.ce && a.nd ? "" : "none"
			},
			Lb: function()
			{
				this.i() ? this.Nb() : this.m()
			},
			jc: function()
			{
				var a = this.e;
				return a.tagName in f.Ac ? a.offsetParent : a
			},
			I: function()
			{
				var a = this.Ta, b;
				if (!a)
				{
					b = this.jc();
					a = this.Ta = doc.createElement("css3-container");
					a.style.direction = "ltr";
					this.Nb();
					b.parentNode.insertBefore(a, b)
				}
				return a
			},
			ab: f.aa,
			m: function()
			{
				var a = this.Ta, b;
				if (a && (b = a.parentNode))
					b.removeChild(a);
				delete this.Ta;
				delete this.ra
			}
		});
		f.Fc = f.u.R({
			N: 2,
			Ya: "background",
			Q: function()
			{
				var a = this.g;
				return a.C.H() || a.G.H()
			},
			i: function()
			{
				var a = this.g;
				return a.q.i() || a.G.i() || a.C.i() || a.ga.i() && a.ga.j().Bb
			},
			V: function()
			{
				var a = this.s.o();
				if (a.h && a.f)
				{
					this.od();
					this.pd()
				}
			},
			od: function()
			{
				var a = this.g.C.j(), b = this.s.o(), c = this.e, d = a && a.color, e, g;
				if (d && d.fa() > 0)
				{
					this.lc();
					a = this.Aa("bgColor", "fill", this.I(), 1);
					e = b.h;
					b = b.f;
					a.stroked = false;
					a.coordsize = e * 2 + "," + b * 2;
					a.coordorigin = "1,1";
					a.path = this.ya(null, 2);
					g = a.style;
					g.width = e;
					g.height = b;
					a.fill.color = d.U(c);
					c = d.fa();
					if (c < 1)
						a.fill.opacity = c
				}
				else
					this.vb("bgColor")
			},
			pd: function()
			{
				var a = this.g.C.j(), b = this.s.o();
				a = a && a.M;
				var c, d, e, g, j;
				if (a)
				{
					this.lc();
					d = b.h;
					e = b.f;
					for (j = a.length; j--;)
					{
						b = a[j];
						c = this.Aa("bgImage" + j, "fill", this.I(), 2);
						c.stroked = false;
						c.fill.type = "tile";
						c.fillcolor = "none";
						c.coordsize = d * 2 + "," + e * 2;
						c.coordorigin = "1,1";
						c.path = this.ya(0, 2);
						g = c.style;
						g.width = d;
						g.height = e;
						if (b.P === "linear-gradient")
							this.bd(c, b);
						else
						{
							c.fill.src = b.Ab;
							this.Nd(c, j)
						}
					}
				}
				for (j = a ? a.length : 0; this.vb("bgImage" + j++);)
					;
			},
			Nd: function(a, b)
			{
				var c = this;
				f.p.Rb(a.fill.src, function(d)
				{
					var e = c.e, g = c.s.o(), j = g.h;
					g = g.f;
					if (j && g)
					{
						var i = a.fill, h = c.g, k = h.w.j(), n = k && k.J;
						k = n ? n.t.a(e) : 0;
						var m = n ? n.r.a(e) : 0, p = n ? n.b.a(e) : 0;
						n = n ? n.l.a(e) : 0;
						h = h.C.j().M[b];
						e = h.$ ? h.$.coords(e, j - d.h - n - m, g - d.f - k - p) : {
							x: 0,
							y: 0
						};
						h = h.bb;
						p = m = 0;
						var r = j + 1, t = g + 1, v = f.O === 8 ? 0 : 1;
						n = Math.round(e.x) + n + 0.5;
						k = Math.round(e.y) + k + 0.5;
						i.position = n / j + "," + k / g;
						i.size.x = 1;
						i.size = d.h + "px," + d.f + "px";
						if (h && h !== "repeat")
						{
							if (h === "repeat-x" || h === "no-repeat")
							{
								m = k + 1;
								t = k + d.f + v
							}
							if (h === "repeat-y" || h === "no-repeat")
							{
								p = n + 1;
								r = n + d.h + v
							}
							a.style.clip = "rect(" + m + "px," + r + "px," + t + "px," + p + "px)"
						}
					}
				})
			},
			bd: function(a, b)
			{
				var c = this.e, d = this.s.o(), e = d.h, g = d.f;
				a = a.fill;
				d = b.ca;
				var j = d.length, i = Math.PI, h = f.Na, k = h.tc, n = h.dc;
				b = h.gc(c, e, g, b);
				h = b.sa;
				var m = b.xc, p = b.yc, r = b.Wd, t = b.Xd, v = b.rd, l = b.sd, q = b.kd, s = b.ld;
				b = b.rc;
				e = h % 90 ? Math.atan2(q * e / g, s) / i * 180 : h + 90;
				e += 180;
				e %= 360;
				v = k(r, t, h, v, l);
				g = n(r, t, v[0], v[1]);
				i = [];
				v = k(m, p, h, r, t);
				n = n(m, p, v[0], v[1]) / g * 100;
				k = [];
				for (h = 0; h < j; h++)
					k.push(d[h].db ? d[h].db.a(c, b) : h === 0 ? 0 : h === j - 1 ? b : null);
				for (h = 1; h < j; h++)
				{
					if (k[h] === null)
					{
						m = k[h - 1];
						b = h;
						do
							p = k[++b];
						while (p === null);
						k[h] = m + (p - m) / (b - h + 1)
					}
					k[h] = Math.max(k[h], k[h - 1])
				}
				for (h = 0; h < j; h++)
					i.push(n + k[h] / g * 100 + "% " + d[h].color.U(c));
				a.angle = e;
				a.type = "gradient";
				a.method = "sigma";
				a.color = d[0].color.U(c);
				a.color2 = d[j - 1].color.U(c);
				if (a.colors)
					a.colors.value = i.join(",");
				else
					a.colors = i.join(",")
			},
			lc: function()
			{
				var a = this.e.runtimeStyle;
				a.backgroundImage = "url(about:blank)";
				a.backgroundColor = "transparent"
			},
			m: function()
			{
				f.u.m.call(this);
				var a = this.e.runtimeStyle;
				a.backgroundImage = a.backgroundColor = ""
			}
		});
		f.Gc = f.u.R({
			N: 4,
			Ya: "border",
			Q: function()
			{
				var a = this.g;
				return a.w.H() || a.G.H()
			},
			i: function()
			{
				var a = this.g;
				return a.G.i() && !a.q.i() && a.w.i()
			},
			V: function()
			{
				var a = this.e, b = this.g.w.j(), c = this.s.o(), d = c.h;
				c = c.f;
				var e, g, j, i, h;
				if (b)
				{
					this.mc();
					b = this.wd(2);
					i = 0;
					for (h = b.length; i < h; i++)
					{
						j = b[i];
						e = this.Aa("borderPiece" + i, j.stroke ? "stroke" : "fill", this.I());
						e.coordsize = d * 2 + "," + c * 2;
						e.coordorigin = "1,1";
						e.path = j.path;
						g = e.style;
						g.width = d;
						g.height = c;
						e.filled = !!j.fill;
						e.stroked = !!j.stroke;
						if (j.stroke)
						{
							e = e.stroke;
							e.weight = j.Qb + "px";
							e.color = j.color.U(a);
							e.dashstyle = j.stroke === "dashed" ? "2 2" : j.stroke === "dotted" ? "1 1" : "solid";
							e.linestyle = j.stroke === "double" && j.Qb > 2 ? "ThinThin" : "Single"
						}
						else
							e.fill.color = j.fill.U(a)
					}
					for (; this.vb("borderPiece" + i++);)
						;
				}
			},
			wd: function(a)
			{
				var b = this.e, c, d, e, g = this.g.w, j = [], i, h, k, n, m = Math.round, p, r, t;
				if (g.i())
				{
					c = g.j();
					g = c.J;
					r = c.Zd;
					t = c.gd;
					if (c.ee && c.$d && c.hd)
					{
						if (t.t.fa() > 0)
						{
							c = g.t.a(b);
							k = c / 2;
							j.push({
								path: this.ya({
									Jb: k,
									Ib: k,
									tb: k,
									Db: k
								}, a),
								stroke: r.t,
								color: t.t,
								Qb: c
							})
						}
					}
					else
					{
						a = a || 1;
						c = this.s.o();
						d = c.h;
						e = c.f;
						c = m(g.t.a(b));
						k = m(g.r.a(b));
						n = m(g.b.a(b));
						b = m(g.l.a(b));
						var v = {
							t: c,
							r: k,
							b: n,
							l: b
						};
						b = this.g.G;
						if (b.i())
							p = this.kc(b.j());
						i = Math.floor;
						h = Math.ceil;
						var l = function(o, u)
						{
							return p ? p[o][u] : 0
						}, q = function(o, u, x, y, z, B)
						{
							var E = l("x", o), D = l("y", o), C = o.charAt(1) === "r";
							o = o.charAt(0) === "b";
							return E > 0 && D > 0 ? (B ? "al" : "ae") + (C ? h(d - E) : i(E)) * a + "," + (o ? h(e - D) : i(D)) * a + "," + (i(E) - u) * a + "," + (i(D) - x) * a
													+ "," + y * 65535 + "," + 2949075 * (z ? 1 : -1) : (B ? "m" : "l") + (C ? d - u : u) * a + "," + (o ? e - x : x) * a
						}, s = function(o, u, x, y)
						{
							var z = o === "t" ? i(l("x", "tl")) * a + "," + h(u) * a : o === "r" ? h(d - u) * a + "," + i(l("y", "tr")) * a : o === "b" ? h(d - l("x", "br")) * a
																																							+ "," + i(e - u) * a
																																						: i(u) * a + ","
																																							+ h(e - l("y", "bl"))
																																							* a;
							o = o === "t" ? h(d - l("x", "tr")) * a + "," + h(u) * a : o === "r" ? h(d - u) * a + "," + h(e - l("y", "br")) * a : o === "b" ? i(l("x", "bl")) * a
																																								+ "," + i(e - u)
																																								* a
																																							: i(u) * a + ","
																																								+ i(l("y", "tl"))
																																								* a;
							return x ? (y ? "m" + o : "") + "l" + z : (y ? "m" + z : "") + "l" + o
						};
						b = function(o, u, x, y, z, B)
						{
							var E = o === "l" || o === "r", D = v[o], C, F;
							if (D > 0 && r[o] !== "none" && t[o].fa() > 0)
							{
								C = v[E ? o : u];
								u = v[E ? u : o];
								F = v[E ? o : x];
								x = v[E ? x : o];
								if (r[o] === "dashed" || r[o] === "dotted")
								{
									j.push({
										path: q(y, C, u, B + 45, 0, 1) + q(y, 0, 0, B, 1, 0),
										fill: t[o]
									});
									j.push({
										path: s(o, D / 2, 0, 1),
										stroke: r[o],
										Qb: D,
										color: t[o]
									});
									j.push({
										path: q(z, F, x, B, 0, 1) + q(z, 0, 0, B - 45, 1, 0),
										fill: t[o]
									})
								}
								else
									j.push({
										path: q(y, C, u, B + 45, 0, 1)
												+ s(o, D, 0, 0)
												+ q(z, F, x, B, 0, 0)
												+ (r[o] === "double" && D > 2 ? q(z, F - i(F / 3), x - i(x / 3), B - 45, 1, 0) + s(o, h(D / 3 * 2), 1, 0)
																				+ q(y, C - i(C / 3), u - i(u / 3), B, 1, 0) + "x " + q(y, i(C / 3), i(u / 3), B + 45, 0, 1)
																				+ s(o, i(D / 3), 1, 0) + q(z, i(F / 3), i(x / 3), B, 0, 0) : "") + q(z, 0, 0, B - 45, 1, 0)
												+ s(o, 0, 1, 0) + q(y, 0, 0, B, 1, 0),
										fill: t[o]
									})
							}
						};
						b("t", "l", "r", "tl", "tr", 90);
						b("r", "t", "b", "tr", "br", 0);
						b("b", "r", "l", "br", "bl", -90);
						b("l", "b", "t", "bl", "tl", -180)
					}
				}
				return j
			},
			m: function()
			{
				if (this.ec || !this.g.q.i())
					this.e.runtimeStyle.borderColor = "";
				f.u.m.call(this)
			}
		});
		f.Tb = f.u.R({
			N: 5,
			Md: ["t", "tr", "r", "br", "b", "bl", "l", "tl", "c"],
			Q: function()
			{
				return this.g.q.H()
			},
			i: function()
			{
				return this.g.q.i()
			},
			V: function()
			{
				this.I();
				var a = this.g.q.j(), b = this.g.w.j(), c = this.s.o(), d = this.e, e = this.uc;
				f.p.Rb(a.src, function(g)
				{
					function j(s, o, u, x, y)
					{
						s = e[s].style;
						var z = Math.max;
						s.width = z(o, 0);
						s.height = z(u, 0);
						s.left = x;
						s.top = y
					}
					function i(s, o, u)
					{
						for (var x = 0, y = s.length; x < y; x++)
							e[s[x]].imagedata[o] = u
					}
					var h = c.h, k = c.f, n = f.n("0"), m = a.J || (b ? b.J : {
						t: n,
						r: n,
						b: n,
						l: n
					});
					n = m.t.a(d);
					var p = m.r.a(d), r = m.b.a(d);
					m = m.l.a(d);
					var t = a.slice, v = t.t.a(d), l = t.r.a(d), q = t.b.a(d);
					t = t.l.a(d);
					j("tl", m, n, 0, 0);
					j("t", h - m - p, n, m, 0);
					j("tr", p, n, h - p, 0);
					j("r", p, k - n - r, h - p, n);
					j("br", p, r, h - p, k - r);
					j("b", h - m - p, r, m, k - r);
					j("bl", m, r, 0, k - r);
					j("l", m, k - n - r, 0, n);
					j("c", h - m - p, k - n - r, m, n);
					i(["tl", "t", "tr"], "cropBottom", (g.f - v) / g.f);
					i(["tl", "l", "bl"], "cropRight", (g.h - t) / g.h);
					i(["bl", "b", "br"], "cropTop", (g.f - q) / g.f);
					i(["tr", "r", "br"], "cropLeft", (g.h - l) / g.h);
					i(["l", "r", "c"], "cropTop", v / g.f);
					i(["l", "r", "c"], "cropBottom", q / g.f);
					i(["t", "b", "c"], "cropLeft", t / g.h);
					i(["t", "b", "c"], "cropRight", l / g.h);
					e.c.style.display = a.fill ? "" : "none"
				}, this)
			},
			I: function()
			{
				var a = this.parent.za(this.N), b, c, d, e = this.Md, g = e.length;
				if (!a)
				{
					a = doc.createElement("border-image");
					b = a.style;
					b.position = "absolute";
					this.uc = {};
					for (d = 0; d < g; d++)
					{
						c = this.uc[e[d]] = f.p.Za("rect");
						c.appendChild(f.p.Za("imagedata"));
						b = c.style;
						b.behavior = "url(#default#VML)";
						b.position = "absolute";
						b.top = b.left = 0;
						c.imagedata.src = this.g.q.j().src;
						c.stroked = false;
						c.filled = false;
						a.appendChild(c)
					}
					this.parent.sb(this.N, a)
				}
				return a
			},
			Ea: function()
			{
				if (this.i())
				{
					var a = this.e, b = a.runtimeStyle, c = this.g.q.j().J;
					b.borderStyle = "solid";
					if (c)
					{
						b.borderTopWidth = c.t.a(a) + "px";
						b.borderRightWidth = c.r.a(a) + "px";
						b.borderBottomWidth = c.b.a(a) + "px";
						b.borderLeftWidth = c.l.a(a) + "px"
					}
					this.mc()
				}
			},
			m: function()
			{
				var a = this.e.runtimeStyle;
				a.borderStyle = "";
				if (this.ec || !this.g.w.i())
					a.borderColor = a.borderWidth = "";
				f.u.m.call(this)
			}
		});
		f.Hc = f.u.R({
			N: 1,
			Ya: "outset-box-shadow",
			Q: function()
			{
				var a = this.g;
				return a.ga.H() || a.G.H()
			},
			i: function()
			{
				var a = this.g.ga;
				return a.i() && a.j().Da[0]
			},
			V: function()
			{
				function a(C, F, O, H, M, P, I)
				{
					C = b.Aa("shadow" + C + F, "fill", d, j - C);
					F = C.fill;
					C.coordsize = n * 2 + "," + m * 2;
					C.coordorigin = "1,1";
					C.stroked = false;
					C.filled = true;
					F.color = M.U(c);
					if (P)
					{
						F.type = "gradienttitle";
						F.color2 = F.color;
						F.opacity = 0
					}
					C.path = I;
					l = C.style;
					l.left = O;
					l.top = H;
					l.width = n;
					l.height = m;
					return C
				}
				var b = this, c = this.e, d = this.I(), e = this.g, g = e.ga.j().Da;
				e = e.G.j();
				var j = g.length, i = j, h, k = this.s.o(), n = k.h, m = k.f;
				k = f.O === 8 ? 1 : 0;
				for (var p = ["tl", "tr", "br", "bl"], r, t, v, l, q, s, o, u, x, y, z, B, E, D; i--;)
				{
					t = g[i];
					q = t.fe.a(c);
					s = t.ge.a(c);
					h = t.Vd.a(c);
					o = t.blur.a(c);
					t = t.color;
					u = -h - o;
					if (!e && o)
						e = f.jb.Dc;
					u = this.ya({
						Jb: u,
						Ib: u,
						tb: u,
						Db: u
					}, 2, e);
					if (o)
					{
						x = (h + o) * 2 + n;
						y = (h + o) * 2 + m;
						z = x ? o * 2 / x : 0;
						B = y ? o * 2 / y : 0;
						if (o - h > n / 2 || o - h > m / 2)
							for (h = 4; h--;)
							{
								r = p[h];
								E = r.charAt(0) === "b";
								D = r.charAt(1) === "r";
								r = a(i, r, q, s, t, o, u);
								v = r.fill;
								v.focusposition = (D ? 1 - z : z) + "," + (E ? 1 - B : B);
								v.focussize = "0,0";
								r.style.clip = "rect(" + ((E ? y / 2 : 0) + k) + "px," + (D ? x : x / 2) + "px," + (E ? y : y / 2) + "px," + ((D ? x / 2 : 0) + k) + "px)"
							}
						else
						{
							r = a(i, "", q, s, t, o, u);
							v = r.fill;
							v.focusposition = z + "," + B;
							v.focussize = 1 - z * 2 + "," + (1 - B * 2)
						}
					}
					else
					{
						r = a(i, "", q, s, t, o, u);
						q = t.fa();
						if (q < 1)
							r.fill.opacity = q
					}
				}
			}
		});
		f.Pc = f.u.R({
			N: 6,
			Ya: "imgEl",
			Q: function()
			{
				var a = this.g;
				return this.e.src !== this.Xc || a.G.H()
			},
			i: function()
			{
				var a = this.g;
				return a.G.i() || a.C.qc()
			},
			V: function()
			{
				this.Xc = j;
				this.Cd();
				var a = this.Aa("img", "fill", this.I()), b = a.fill, c = this.s.o(), d = c.h;
				c = c.f;
				var e = this.g.w.j(), g = e && e.J;
				e = this.e;
				var j = e.src, i = Math.round, h = e.currentStyle, k = f.n;
				if (!g || f.O < 7)
				{
					g = f.n("0");
					g = {
						t: g,
						r: g,
						b: g,
						l: g
					}
				}
				a.stroked = false;
				b.type = "frame";
				b.src = j;
				b.position = (d ? 0.5 / d : 0) + "," + (c ? 0.5 / c : 0);
				a.coordsize = d * 2 + "," + c * 2;
				a.coordorigin = "1,1";
				a.path = this.ya({
					Jb: i(g.t.a(e) + k(h.paddingTop).a(e)),
					Ib: i(g.r.a(e) + k(h.paddingRight).a(e)),
					tb: i(g.b.a(e) + k(h.paddingBottom).a(e)),
					Db: i(g.l.a(e) + k(h.paddingLeft).a(e))
				}, 2);
				a = a.style;
				a.width = d;
				a.height = c
			},
			Cd: function()
			{
				this.e.runtimeStyle.filter = "alpha(opacity=0)"
			},
			m: function()
			{
				f.u.m.call(this);
				this.e.runtimeStyle.filter = ""
			}
		});
		f.Oc = f.u.R({
			ib: f.aa,
			Mb: f.aa,
			Nb: f.aa,
			Lb: f.aa,
			Ld: /^,+|,+$/g,
			Fd: /,+/g,
			gb: function(a, b)
			{
				(this.pb || (this.pb = []))[a] = b || void 0
			},
			ab: function()
			{
				var a = this.pb, b;
				if (a && (b = a.join(",").replace(this.Ld, "").replace(this.Fd, ",")) !== this.Wc)
					this.Wc = this.e.runtimeStyle.background = b
			},
			m: function()
			{
				this.e.runtimeStyle.background = "";
				delete this.pb
			}
		});
		f.Mc = f.u.R({
			ua: 1,
			Q: function()
			{
				return this.g.C.H()
			},
			i: function()
			{
				var a = this.g;
				return a.C.i() || a.q.i()
			},
			V: function()
			{
				var a = this.g.C.j(), b, c, d = 0, e, g;
				if (a)
				{
					b = [];
					if (c = a.M)
						for (; e = c[d++];)
							if (e.P === "linear-gradient")
							{
								g = this.vd(e.Wa);
								g = (e.Xa || f.Ka.Kc).a(this.e, g.h, g.f, g.h, g.f);
								b.push("url(data:image/svg+xml," + escape(this.xd(e, g.h, g.f)) + ") " + this.dd(e.$) + " / " + g.h + "px " + g.f + "px " + (e.bc || "") + " "
										+ (e.Wa || "") + " " + (e.ub || ""))
							}
							else
								b.push(e.Hb);
					a.color && b.push(a.color.Y);
					this.parent.gb(this.ua, b.join(","))
				}
			},
			dd: function(a)
			{
				return a ? a.X.map(function(b)
				{
					return b.d
				}).join(" ") : "0 0"
			},
			vd: function(a)
			{
				var b = this.e, c = this.s.o(), d = c.h;
				c = c.f;
				var e;
				if (a !== "border-box")
					if ((e = this.g.w.j()) && (e = e.J))
					{
						d -= e.l.a(b) + e.l.a(b);
						c -= e.t.a(b) + e.b.a(b)
					}
				if (a === "content-box")
				{
					a = f.n;
					e = b.currentStyle;
					d -= a(e.paddingLeft).a(b) + a(e.paddingRight).a(b);
					c -= a(e.paddingTop).a(b) + a(e.paddingBottom).a(b)
				}
				return {
					h: d,
					f: c
				}
			},
			xd: function(a, b, c)
			{
				var d = this.e, e = a.ca, g = e.length, j = f.Na.gc(d, b, c, a);
				a = j.xc;
				var i = j.yc, h = j.td, k = j.ud;
				j = j.rc;
				var n, m, p, r, t;
				n = [];
				for (m = 0; m < g; m++)
					n.push(e[m].db ? e[m].db.a(d, j) : m === 0 ? 0 : m === g - 1 ? j : null);
				for (m = 1; m < g; m++)
					if (n[m] === null)
					{
						r = n[m - 1];
						p = m;
						do
							t = n[++p];
						while (t === null);
						n[m] = r + (t - r) / (p - m + 1)
					}
				b = ['<svg width="' + b + '" height="' + c + '" xmlns="http://www.w3.org/2000/svg"><defs><linearGradient id="g" gradientUnits="userSpaceOnUse" x1="' + a / b * 100
						+ '%" y1="' + i / c * 100 + '%" x2="' + h / b * 100 + '%" y2="' + k / c * 100 + '%">'];
				for (m = 0; m < g; m++)
					b.push('<stop offset="' + n[m] / j + '" stop-color="' + e[m].color.U(d) + '" stop-opacity="' + e[m].color.fa() + '"/>');
				b.push('</linearGradient></defs><rect width="100%" height="100%" fill="url(#g)"/></svg>');
				return b.join("")
			},
			m: function()
			{
				this.parent.gb(this.ua)
			}
		});
		f.Nc = f.u.R({
			T: "repeat",
			Sc: "stretch",
			Qc: "round",
			ua: 0,
			Q: function()
			{
				return this.g.q.H()
			},
			i: function()
			{
				return this.g.q.i()
			},
			V: function()
			{
				var a = this, b = a.g.q.j(), c = a.g.w.j(), d = a.s.o(), e = b.repeat, g = e.f, j = e.Ob, i = a.e, h = 0;
				f.p.Rb(b.src, function(k)
				{
					function n(Q, R, U, V, W, Y, X, S, w, A)
					{
						K.push('<pattern patternUnits="userSpaceOnUse" id="pattern' + G + '" x="' + (g === l ? Q + U / 2 - w / 2 : Q) + '" y="' + (j === l ? R + V / 2 - A / 2 : R)
								+ '" width="' + w + '" height="' + A + '"><svg width="' + w + '" height="' + A + '" viewBox="' + W + " " + Y + " " + X + " " + S
								+ '" preserveAspectRatio="none"><image xlink:href="' + v + '" x="0" y="0" width="' + r + '" height="' + t + '" /></svg></pattern>');
						J.push('<rect x="' + Q + '" y="' + R + '" width="' + U + '" height="' + V + '" fill="url(#pattern' + G + ')" />');
						G++
					}
					var m = d.h, p = d.f, r = k.h, t = k.f, v = a.Dd(b.src, r, t), l = a.T, q = a.Sc;
					k = a.Qc;
					var s = Math.ceil, o = f.n("0"), u = b.J || (c ? c.J : {
						t: o,
						r: o,
						b: o,
						l: o
					});
					o = u.t.a(i);
					var x = u.r.a(i), y = u.b.a(i);
					u = u.l.a(i);
					var z = b.slice, B = z.t.a(i), E = z.r.a(i), D = z.b.a(i);
					z = z.l.a(i);
					var C = m - u - x, F = p - o - y, O = r - z - E, H = t - B - D, M = g === q ? C : O * o / B, P = j === q ? F : H * x / E, I = g === q ? C : O * y / D;
					q = j === q ? F : H * u / z;
					var K = [], J = [], G = 0;
					if (g === k)
					{
						M -= (M - (C % M || M)) / s(C / M);
						I -= (I - (C % I || I)) / s(C / I)
					}
					if (j === k)
					{
						P -= (P - (F % P || P)) / s(F / P);
						q -= (q - (F % q || q)) / s(F / q)
					}
					k = ['<svg width="' + m + '" height="' + p + '" xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink">'];
					n(0, 0, u, o, 0, 0, z, B, u, o);
					n(u, 0, C, o, z, 0, O, B, M, o);
					n(m - x, 0, x, o, r - E, 0, E, B, x, o);
					n(0, o, u, F, 0, B, z, H, u, q);
					if (b.fill)
						n(u, o, C, F, z, B, O, H, M || I || O, q || P || H);
					n(m - x, o, x, F, r - E, B, E, H, x, P);
					n(0, p - y, u, y, 0, t - D, z, D, u, y);
					n(u, p - y, C, y, z, t - D, O, D, I, y);
					n(m - x, p - y, x, y, r - E, t - D, E, D, x, y);
					k.push("<defs>" + K.join("\n") + "</defs>" + J.join("\n") + "</svg>");
					a.parent.gb(a.ua, "url(data:image/svg+xml," + escape(k.join("")) + ") no-repeat border-box border-box");
					h && a.parent.ab()
				}, a);
				h = 1
			},
			Dd: function()
			{
				var a = {};
				return function(b, c, d)
				{
					var e = a[b], g;
					if (!e)
					{
						e = new Image;
						g = doc.createElement("canvas");
						e.src = b;
						g.width = c;
						g.height = d;
						g.getContext("2d").drawImage(e, 0, 0);
						e = a[b] = g.toDataURL()
					}
					return e
				}
			}(),
			Ea: f.Tb.prototype.Ea,
			m: function()
			{
				var a = this.e.runtimeStyle;
				this.parent.gb(this.ua);
				a.borderColor = a.borderStyle = a.borderWidth = ""
			}
		});
		f.kb = function()
		{
			function a(l, q)
			{
				l.className += " " + q
			}
			function b(l)
			{
				var q = v.slice.call(arguments, 1), s = q.length;
				setTimeout(function()
				{
					if (l)
						for (; s--;)
							a(l, q[s])
				}, 0)
			}
			function c(l)
			{
				var q = v.slice.call(arguments, 1), s = q.length;
				setTimeout(function()
				{
					if (l)
						for (; s--;)
						{
							var o = q[s];
							o = t[o] || (t[o] = new RegExp("\\b" + o + "\\b", "g"));
							l.className = l.className.replace(o, "")
						}
				}, 0)
			}
			function d(l)
			{
				function q()
				{
					if (!U)
					{
						var w, A, L = f.ja, T = l.currentStyle, N = T.getAttribute(g) === "true", da = T.getAttribute(i) !== "false", ea = T.getAttribute(h) !== "false";
						S = T.getAttribute(j);
						S = L > 7 ? S !== "false" : S === "true";
						if (!R)
						{
							R = 1;
							l.runtimeStyle.zoom = 1;
							T = l;
							for (var fa = 1; T = T.previousSibling;)
								if (T.nodeType === 1)
								{
									fa = 0;
									break
								}
							fa && a(l, p)
						}
						J.cb();
						if (N && (A = J.o()) && (w = doc.documentElement || doc.body) && (A.y > w.clientHeight || A.x > w.clientWidth || A.y + A.f < 0 || A.x + A.h < 0))
						{
							if (!Y)
							{
								Y = 1;
								f.mb.ba(q)
							}
						}
						else
						{
							U = 1;
							Y = R = 0;
							f.mb.Ha(q);
							if (L === 9)
							{
								G = {
									C: new f.Sb(l),
									q: new f.Ub(l),
									w: new f.Vb(l)
								};
								Q = [G.C, G.q];
								K = new f.Oc(l, J, G);
								w = [new f.Mc(l, J, G, K), new f.Nc(l, J, G, K)]
							}
							else
							{
								G = {
									C: new f.Sb(l),
									w: new f.Vb(l),
									q: new f.Ub(l),
									G: new f.jb(l),
									ga: new f.Ic(l),
									Pb: new f.Uc(l)
								};
								Q = [G.C, G.w, G.q, G.G, G.ga, G.Pb];
								K = new f.Rc(l, J, G);
								w = [new f.Hc(l, J, G, K), new f.Fc(l, J, G, K), new f.Gc(l, J, G, K), new f.Tb(l, J, G, K)];
								l.tagName === "IMG" && w.push(new f.Pc(l, J, G, K));
								K.ed = w
							}
							I = [K].concat(w);
							if (w = l.currentStyle.getAttribute(f.F + "watch-ancestors"))
							{
								w = parseInt(w, 10);
								A = 0;
								for (N = l.parentNode; N && (w === "NaN" || A++ < w);)
								{
									H(N, "onpropertychange", C);
									H(N, "onmouseenter", x);
									H(N, "onmouseleave", y);
									H(N, "onmousedown", z);
									if (N.tagName in f.fc)
									{
										H(N, "onfocus", E);
										H(N, "onblur", D)
									}
									N = N.parentNode
								}
							}
							if (S)
							{
								f.Oa.ba(o);
								f.Oa.Rd()
							}
							o(1)
						}
						if (!V)
						{
							V = 1;
							L < 9 && H(l, "onmove", s);
							H(l, "onresize", s);
							H(l, "onpropertychange", u);
							ea && H(l, "onmouseenter", x);
							if (ea || da)
								H(l, "onmouseleave", y);
							da && H(l, "onmousedown", z);
							if (l.tagName in f.fc)
							{
								H(l, "onfocus", E);
								H(l, "onblur", D)
							}
							f.Qa.ba(s);
							f.L.ba(M)
						}
						J.hb()
					}
				}
				function s()
				{
					J && J.Ad() && o()
				}
				function o(w)
				{
					if (!X)
						if (U)
						{
							var A, L = I.length;
							F();
							for (A = 0; A < L; A++)
								I[A].Ea();
							if (w || J.Od())
								for (A = 0; A < L; A++)
									I[A].ib();
							if (w || J.Td())
								for (A = 0; A < L; A++)
									I[A].Mb();
							K.ab();
							O()
						}
						else
							R || q()
				}
				function u()
				{
					var w, A = I.length, L;
					w = event;
					if (!X && !(w && w.propertyName in r))
						if (U)
						{
							F();
							for (w = 0; w < A; w++)
								I[w].Ea();
							for (w = 0; w < A; w++)
							{
								L = I[w];
								L.Cb || L.ib();
								L.Q() && L.Lb()
							}
							K.ab();
							O()
						}
						else
							R || q()
				}
				function x()
				{
					b(l, k)
				}
				function y()
				{
					c(l, k, n)
				}
				function z()
				{
					b(l, n);
					f.lb.ba(B)
				}
				function B()
				{
					c(l, n);
					f.lb.Ha(B)
				}
				function E()
				{
					b(l, m)
				}
				function D()
				{
					c(l, m)
				}
				function C()
				{
					var w = event.propertyName;
					if (w === "className" || w === "id")
						u()
				}
				function F()
				{
					J.cb();
					for (var w = Q.length; w--;)
						Q[w].cb()
				}
				function O()
				{
					for (var w = Q.length; w--;)
						Q[w].hb();
					J.hb()
				}
				function H(w, A, L)
				{
					w.attachEvent(A, L);
					W.push([w, A, L])
				}
				function M()
				{
					if (V)
					{
						for (var w = W.length, A; w--;)
						{
							A = W[w];
							A[0].detachEvent(A[1], A[2])
						}
						f.L.Ha(M);
						V = 0;
						W = []
					}
				}
				function P()
				{
					if (!X)
					{
						var w, A;
						M();
						X = 1;
						if (I)
						{
							w = 0;
							for (A = I.length; w < A; w++)
							{
								I[w].ec = 1;
								I[w].m()
							}
						}
						S && f.Oa.Ha(o);
						f.Qa.Ha(o);
						I = J = G = Q = l = null
					}
				}
				var I, K, J = new ha(l), G, Q, R, U, V, W = [], Y, X, S;
				this.Ed = q;
				this.update = o;
				this.m = P;
				this.qd = l
			}
			var e = {}, g = f.F + "lazy-init", j = f.F + "poll", i = f.F + "track-active", h = f.F + "track-hover", k = f.La + "hover", n = f.La + "active", m = f.La + "focus", p = f.La
																																														+ "first-child", r = {
				background: 1,
				bgColor: 1,
				display: 1
			}, t = {}, v = [];
			d.yd = function(l)
			{
				var q = f.p.Ba(l);
				return e[q] || (e[q] = new d(l))
			};
			d.m = function(l)
			{
				l = f.p.Ba(l);
				var q = e[l];
				if (q)
				{
					q.m();
					delete e[l]
				}
			};
			d.md = function()
			{
				var l = [], q;
				if (e)
				{
					for ( var s in e)
						if (e.hasOwnProperty(s))
						{
							q = e[s];
							l.push(q.qd);
							q.m()
						}
					e = {}
				}
				return l
			};
			return d
		}();
		f.supportsVML = f.zc;
		f.attach = function(a)
		{
			f.ja < 10 && f.zc && f.kb.yd(a).Ed()
		};
		f.detach = function(a)
		{
			f.kb.m(a)
		}
	}
	;
})();
