package nl.rivm.screenit.specification.colon;

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

import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.model.TablePerClassHibernateObject_;
import nl.rivm.screenit.model.colon.ColonDossier;
import nl.rivm.screenit.model.colon.ColonDossier_;
import nl.rivm.screenit.model.colon.ColonUitnodigingsinterval_;
import nl.rivm.screenit.model.colon.ColonVolgendeUitnodiging_;
import nl.rivm.screenit.specification.ExtendedSpecification;

import static nl.rivm.screenit.specification.SpecificationUtil.join;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class ColonDossierSpecification
{
	public static ExtendedSpecification<ColonDossier> heeftGeenGevuldDossier()
	{
		return (r, q, cb) ->
			cb.or(cb.isNull(r.get(TablePerClassHibernateObject_.id)), cb.and(cb.isNull(r.get(ColonDossier_.laatsteScreeningRonde)), cb.isTrue(r.get(Dossier_.aangemeld))));
	}

	public static ExtendedSpecification<ColonDossier> heeftVolgendeUitnodigingNaInterval()
	{
		return (r, q, cb) ->
		{
			var volgendeUitnodiging = join(r, ColonDossier_.volgendeUitnodiging);
			var intervalJoin = join(volgendeUitnodiging, ColonVolgendeUitnodiging_.interval);
			return cb.greaterThan(volgendeUitnodiging.get(ColonVolgendeUitnodiging_.peildatum), intervalJoin.get(ColonUitnodigingsinterval_.berekendeReferentieDatum));
		};
	}
}
