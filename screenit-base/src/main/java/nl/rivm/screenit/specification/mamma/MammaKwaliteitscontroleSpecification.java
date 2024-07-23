package nl.rivm.screenit.specification.mamma;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2024 Facilitaire Samenwerking Bevolkingsonderzoek
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
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek;
import nl.rivm.screenit.model.mamma.MammaAdhocMeekijkverzoek_;
import nl.rivm.screenit.model.mamma.MammaAfspraak_;
import nl.rivm.screenit.model.mamma.MammaBeoordeling_;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek;
import nl.rivm.screenit.model.mamma.MammaFotobesprekingOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaOnderzoek_;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.MammaUitnodiging;
import nl.rivm.screenit.model.mamma.MammaUitnodiging_;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek_;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaKwaliteitscontroleSpecification
{
	public static Specification<MammaFotobesprekingOnderzoek> fotoBesprekingHeeftScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		return heeftScreeningRonde(screeningRonde).toSpecification(r ->
		{
			var beoordelingJoin = join(r, MammaFotobesprekingOnderzoek_.beoordeling);
			var onderzoekJoin = join(beoordelingJoin, MammaBeoordeling_.onderzoek);
			var afspraakJoin = join(onderzoekJoin, MammaOnderzoek_.afspraak);
			return join(afspraakJoin, MammaAfspraak_.uitnodiging);
		});
	}

	public static Specification<MammaVisitatieOnderzoek> visitatieHeeftScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		return heeftScreeningRonde(screeningRonde).toSpecification(r ->
		{
			var beoordelingJoin = join(r, MammaVisitatieOnderzoek_.beoordeling);
			var onderzoekJoin = join(beoordelingJoin, MammaBeoordeling_.onderzoek);
			var afspraakJoin = join(onderzoekJoin, MammaOnderzoek_.afspraak);
			return join(afspraakJoin, MammaAfspraak_.uitnodiging);
		});
	}

	public static Specification<MammaAdhocMeekijkverzoek> adhocMeekijkverzoekHeeftScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		return heeftScreeningRonde(screeningRonde).toSpecification(r ->
		{
			var onderzoekJoin = join(r, MammaAdhocMeekijkverzoek_.onderzoek);
			var afspraakJoin = join(onderzoekJoin, MammaOnderzoek_.afspraak);
			return join(afspraakJoin, MammaAfspraak_.uitnodiging);
		});
	}

	public static PathAwarePredicate<MammaUitnodiging> heeftScreeningRonde(MammaScreeningRonde screeningRonde)
	{
		return (cb, r) -> cb.equal(r.get(MammaUitnodiging_.screeningRonde), screeningRonde);
	}
}
