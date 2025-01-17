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
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek_;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderdeel;
import nl.rivm.screenit.model.mamma.enums.MammaVisitatieOnderzoekStatus;

import org.springframework.data.jpa.domain.Specification;

import static org.springframework.data.jpa.domain.Specification.not;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaVisitatieOnderzoekSpecification
{
	public static Specification<MammaVisitatieOnderzoek> heeftBeoordeling(MammaBeoordeling beoordeling)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaVisitatieOnderzoek_.beoordeling), beoordeling);
	}

	public static Specification<MammaVisitatieOnderzoek> heeftVisitatie(MammaVisitatie visitatie)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaVisitatieOnderzoek_.visitatie), visitatie);
	}

	public static Specification<MammaVisitatieOnderzoek> heeftStatus(MammaVisitatieOnderzoekStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaVisitatieOnderzoek_.status), status);
	}

	public static Specification<MammaVisitatieOnderzoek> heeftNietStatus(MammaVisitatieOnderzoekStatus status)
	{
		return not(heeftStatus(status));
	}

	public static Specification<MammaVisitatieOnderzoek> heeftOnderdeel(MammaVisitatieOnderdeel onderdeel)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaVisitatieOnderzoek_.onderdeel), onderdeel);
	}
}
