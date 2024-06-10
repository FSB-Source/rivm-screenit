package nl.rivm.screenit.specification.algemeen;

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

import java.time.LocalDate;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.specification.SpecificationUtil;
import nl.rivm.screenit.specification.mamma.MammaDossierSpecification;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.functionalinterfaces.EntityAwarePredicate;
import nl.rivm.screenit.util.functionalinterfaces.PathAwarePredicate;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ClientSpecification
{
	public static PathAwarePredicate<Client> heeftIndicatie()
	{
		return (cb, r) ->
			cb.equal(r.get(Client_.gbaStatus), GbaStatus.INDICATIE_AANWEZIG);
	}

	public static EntityAwarePredicate<Client> heeftActieveClientPredicate()
	{
		return (cb, r) ->
		{
			var persoon = SpecificationUtil.join(r, Client_.persoon);
			var heeftGeenOverledenDatum = PersoonSpecification.heeftGeenOverledenDatumPredicate().withPath(cb, persoon);
			var heeftGeenVertrokkenUitNederlandDatum = PersoonSpecification.heeftGeenVertrokkenUitNederlandDatumPredicate().withPath(cb, persoon);
			var heeftIndicatie = heeftIndicatie().withPath(cb, r);
			return cb.and(heeftGeenOverledenDatum, heeftGeenVertrokkenUitNederlandDatum, heeftIndicatie);
		};
	}

	public static Specification<Client> heeftGeboorteJaarVoorLeeftijdBereik(int minimaleLeeftijd, int maximaleLeeftijd, LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var persoon = SpecificationUtil.join(r, Client_.persoon);
			var geboorteDatum = persoon.get(GbaPersoon_.geboortedatum);
			var maxGeboortedatum = DateUtil.toUtilDate(LocalDate.of(peildatum.getYear() - minimaleLeeftijd, 12, 31));
			var minGeboortedatum = DateUtil.toUtilDate(LocalDate.of(peildatum.getYear() - maximaleLeeftijd, 1, 1));
			return cb.and(cb.greaterThanOrEqualTo(geboorteDatum, minGeboortedatum), cb.lessThanOrEqualTo(geboorteDatum, maxGeboortedatum));
		};
	}

	public static Specification<Client> woontNietInTehuis()
	{
		return MammaDossierSpecification.woontNietInTehuisPredicate().toSpecification(r -> SpecificationUtil.join(r, Client_.mammaDossier));
	}
}
