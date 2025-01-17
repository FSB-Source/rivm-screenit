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

import java.time.LocalDateTime;
import java.util.Collection;

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.mamma.MammaDossier;
import nl.rivm.screenit.model.mamma.MammaDossier_;
import nl.rivm.screenit.model.mamma.enums.MammaDoelgroep;
import nl.rivm.screenit.specification.ExtendedSpecification;
import nl.rivm.screenit.specification.StringLiteral;
import nl.rivm.screenit.util.DateUtil;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class MammaBaseDossierSpecification
{
	public static ExtendedSpecification<MammaDossier> woontInTehuis()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(MammaDossier_.tehuis));
	}

	public static ExtendedSpecification<MammaDossier> woontNietInTehuis()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaDossier_.tehuis));
	}

	public static ExtendedSpecification<MammaDossier> heeftDoelgroepIn(Collection<MammaDoelgroep> doelgroepen)
	{
		return (r, q, cb) -> r.get(MammaDossier_.doelgroep).in(doelgroepen);
	}

	public static ExtendedSpecification<MammaDossier> heeftScreeningRondeEvent()
	{
		return (r, q, cb) -> cb.isNotNull(r.get(MammaDossier_.screeningRondeEvent));
	}

	public static ExtendedSpecification<MammaDossier> heeftGeenScreeningRondeEvent()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaDossier_.screeningRondeEvent));
	}

	public static ExtendedSpecification<MammaDossier> heeftStatusNullOfActief()
	{
		return (r, q, cb) -> cb.equal(cb.coalesce(r.get(Dossier_.status), new StringLiteral(cb, DossierStatus.ACTIEF.name())),
			DossierStatus.ACTIEF);
	}

	public static ExtendedSpecification<MammaDossier> heeftNooitMammografieGehad()
	{
		return (r, q, cb) -> cb.isNull(r.get(MammaDossier_.laatsteMammografieAfgerond));
	}

	public static ExtendedSpecification<MammaDossier> heeftLaatsteMammografieAfgerondVoorMoment(LocalDateTime peilMoment)
	{
		return (r, q, cb) -> cb.lessThan(r.get(MammaDossier_.laatsteMammografieAfgerond), DateUtil.toUtilDate(peilMoment));
	}

	public static ExtendedSpecification<MammaDossier> isUpdateFollowUpConclusie(boolean updateFollowUpConclusie)
	{
		return (r, q, cb) -> cb.equal(r.get(MammaDossier_.updateFollowUpConclusie), updateFollowUpConclusie);
	}

	public static ExtendedSpecification<MammaDossier> heeftEersteOnderzoek()
	{
		return (r, q, cb) -> cb.isTrue(r.get(MammaDossier_.eersteOnderzoek));
	}
}
