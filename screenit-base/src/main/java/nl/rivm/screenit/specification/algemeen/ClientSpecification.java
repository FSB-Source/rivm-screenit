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
import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.Client_;
import nl.rivm.screenit.model.GbaPersoon_;
import nl.rivm.screenit.model.enums.GbaStatus;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.util.DateUtil;

import org.apache.commons.lang.StringUtils;
import org.springframework.data.jpa.domain.Specification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.isNietOverleden;
import static nl.rivm.screenit.specification.algemeen.PersoonSpecification.woontInNederland;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class ClientSpecification
{
	public static ExtendedSpecification<Client> heeftIndicatie()
	{
		return (r, q, cb) -> cb.equal(r.get(Client_.gbaStatus), GbaStatus.INDICATIE_AANWEZIG);
	}

	public static ExtendedSpecification<Client> heeftActieveClient()
	{
		return isNietOverleden().with(Client_.persoon)
			.and(woontInNederland().with(Client_.persoon))
			.and(heeftIndicatie());
	}

	public static Specification<Client> heeftGeboorteJaarVoorLeeftijdBereik(int minimaleLeeftijd, int maximaleLeeftijd, LocalDate peildatum)
	{
		return (r, q, cb) ->
		{
			var persoon = join(r, Client_.persoon);
			var geboorteDatum = persoon.get(GbaPersoon_.geboortedatum);
			var maxGeboortedatum = DateUtil.toUtilDate(LocalDate.of(peildatum.getYear() - minimaleLeeftijd, 12, 31));
			var minGeboortedatum = DateUtil.toUtilDate(LocalDate.of(peildatum.getYear() - maximaleLeeftijd, 1, 1));
			return cb.and(cb.greaterThanOrEqualTo(geboorteDatum, minGeboortedatum), cb.lessThanOrEqualTo(geboorteDatum, maxGeboortedatum));
		};
	}

	public static Specification<Client> heeftANummer(String anummer)
	{
		return (r, q, cb) ->
		{
			var persoonJoin = join(r, Client_.persoon);
			if (StringUtils.isNotEmpty(anummer))
			{
				return cb.equal(persoonJoin.get(GbaPersoon_.anummer), anummer);
			}
			return null;
		};
	}

	public static Specification<Client> heeftTitelCode(String titelCode)
	{
		return (r, q, cb) ->
		{
			var persoonJoin = join(r, Client_.persoon);
			return cb.equal(persoonJoin.get(GbaPersoon_.titelCode), titelCode);
		};
	}

	public static Specification<Client> heeftGbaMutaties()
	{
		return (r, q, cb) -> cb.notEqual(cb.size(r.get(Client_.gbaMutaties)), 0);
	}

	public static Specification<Client> heeftBsnDieEindigtMet(String bsn)
	{
		return (r, q, cb) ->
		{
			var persoonJoin = join(r, Client_.persoon);
			return cb.like(persoonJoin.get(GbaPersoon_.bsn), "%" + bsn);
		};
	}

	public static Specification<Client> heeftGbaStatus(GbaStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(Client_.gbaStatus), status);
	}

	public static Specification<Client> heeftNietGbaStatus(GbaStatus status)
	{
		return ((r, q, cb) -> cb.notEqual(r.get(Client_.gbaStatus), status));
	}

	public static Specification<Client> heeftNietGbaStatussen(List<GbaStatus> statussen)
	{
		return (r, q, cb) -> cb.not(r.get(Client_.gbaStatus).in(statussen));
	}

	public static ExtendedSpecification<Client> heeftId(Long id)
	{
		return (r, q, cb) -> cb.equal(r.get(Client_.id), id);
	}
}
