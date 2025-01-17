package nl.rivm.screenit.specification.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaBeoordeling;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek_;
import nl.rivm.screenit.model.mamma.enums.MammaFotobesprekingOnderzoekStatus;

import org.springframework.data.jpa.domain.Specification;

import static org.springframework.data.jpa.domain.Specification.not;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaFotobesprekingOnderzoekSpecification
{
	public static Specification<MammaFotobesprekingOnderzoek> heeftStatus(MammaFotobesprekingOnderzoekStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaFotobesprekingOnderzoek_.status), status);
	}

	public static Specification<MammaFotobesprekingOnderzoek> heeftNietStatus(MammaFotobesprekingOnderzoekStatus status)
	{
		return not(heeftStatus(status));
	}

	public static Specification<MammaFotobesprekingOnderzoek> heeftBeoordeling(MammaBeoordeling beoordeling)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaFotobesprekingOnderzoek_.beoordeling), beoordeling);
	}

	public static Specification<MammaFotobesprekingOnderzoek> heeftFotobespreking(MammaFotobespreking fotobespreking)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaFotobesprekingOnderzoek_.fotobespreking), fotobespreking);
	}
}
