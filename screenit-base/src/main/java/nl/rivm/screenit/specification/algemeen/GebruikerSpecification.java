package nl.rivm.screenit.specification.algemeen;

/*-
 * ========================LICENSE_START=================================
 * screenit-base
 * %%
 * Copyright (C) 2012 - 2023 Facilitaire Samenwerking Bevolkingsonderzoek
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
import java.util.Date;
import java.util.List;

import lombok.AccessLevel;
import lombok.AllArgsConstructor;

import nl.rivm.screenit.Constants;
import nl.rivm.screenit.model.Gebruiker;
import nl.rivm.screenit.model.Gebruiker_;
import nl.rivm.screenit.model.InlogStatus;
import nl.rivm.screenit.model.enums.InlogMethode;
import nl.rivm.screenit.util.DateUtil;
import nl.rivm.screenit.util.criteriabuilder.ScreenitCriteriaBuilderImpl;

import org.springframework.data.jpa.domain.Specification;

@AllArgsConstructor(access = AccessLevel.PRIVATE)
public class GebruikerSpecification
{
	private static final Date BEGIN_OF_TIME = DateUtil.parseDateForPattern("01-01-1900", Constants.DEFAULT_DATE_FORMAT);

	private static final Date END_OF_TIME = DateUtil.parseDateForPattern("01-01-4000", Constants.DEFAULT_DATE_FORMAT);

	public static Specification<Gebruiker> isActieveGebruiker(LocalDate vandaag, int dagenWachtwoordGeldig)
	{
		return (r, q, cb) ->
		{
			var scb = new ScreenitCriteriaBuilderImpl(cb);
			var vandaagDate = DateUtil.toUtilDate(vandaag);
			var peildatumLaatstGewijzigdDate = DateUtil.toUtilDate(vandaag.minusDays(dagenWachtwoordGeldig));
			var rightInterval = "+ '" + dagenWachtwoordGeldig + " days' ";

			var vanafExpression = cb.coalesce(r.get(Gebruiker_.actiefVanaf), BEGIN_OF_TIME);
			var totEnMetExpression = cb.coalesce(r.get(Gebruiker_.actiefTotEnMet), END_OF_TIME);
			var actiefTijdensVerlopenVanWachtwoord =
				cb.and(
					scb.lessThanOrEqualTo(vanafExpression, r.get(Gebruiker_.laatsteKeerWachtwoordGewijzigd), rightInterval),
					scb.greaterThanOrEqualTo(totEnMetExpression, r.get(Gebruiker_.laatsteKeerWachtwoordGewijzigd), rightInterval));
			var nuActief =
				cb.and(
					cb.lessThanOrEqualTo(vanafExpression, vandaagDate),
					cb.greaterThanOrEqualTo(totEnMetExpression, vandaagDate));

			var wachtwoordVerlooptInDeToekomst = cb.greaterThan(r.get(Gebruiker_.laatsteKeerWachtwoordGewijzigd), peildatumLaatstGewijzigdDate);
			return
				cb.and(
					cb.isTrue(r.get(Gebruiker_.actief)),
					cb.or(
						cb.and(
							wachtwoordVerlooptInDeToekomst,
							actiefTijdensVerlopenVanWachtwoord),
						cb.and(
							cb.not(wachtwoordVerlooptInDeToekomst),
							nuActief))
				);
		};
	}

	public static Specification<Gebruiker> heeftEmailAdres()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(Gebruiker_.emailextra));
	}

	public static Specification<Gebruiker> heeftWachtwoordInlogMethode()
	{
		return (r, q, cb) -> r.get(Gebruiker_.inlogMethode).in(List.of(InlogMethode.YUBIKEY, InlogMethode.GEBRUIKERSNAAM_WACHTWOORD));
	}

	public static Specification<Gebruiker> moetHerinneringKrijgen(LocalDate laatsteKeerWachtwoordGewijzigdPeildatum)
	{
		return (r, q, cb) ->
			cb.and(
				cb.lessThan(r.get(Gebruiker_.laatsteKeerWachtwoordGewijzigd), DateUtil.toUtilDate(laatsteKeerWachtwoordGewijzigdPeildatum)),
				cb.equal(r.get(Gebruiker_.wachtwoordVerlooptWaarschuwingVerzonden), false)
			);
	}

	public static Specification<Gebruiker> isNietGeblokkeerd()
	{
		return (r, q, cb) -> cb.notEqual(r.get(Gebruiker_.inlogstatus), InlogStatus.GEBLOKKEERD);
	}
}
