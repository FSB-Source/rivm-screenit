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

import lombok.AccessLevel;
import lombok.NoArgsConstructor;

import nl.rivm.screenit.model.Dossier;
import nl.rivm.screenit.model.DossierStatus;
import nl.rivm.screenit.model.Dossier_;
import nl.rivm.screenit.specification.ExtendedSpecification;

@NoArgsConstructor(access = AccessLevel.PRIVATE)
public class DossierSpecification
{
	public static <D extends Dossier<?, ?>> ExtendedSpecification<D> heeftStatus(DossierStatus status)
	{
		return (r, q, cb) -> cb.equal(r.get(Dossier_.status), status);
	}

	public static <D extends Dossier<?, ?>> ExtendedSpecification<D> isAangemeld(Boolean aangemeld)
	{
		return (r, q, cb) -> cb.equal(r.get(Dossier_.aangemeld), aangemeld);
	}

	public static <D extends Dossier<?, ?>> ExtendedSpecification<D> wachtOpStartProject(Boolean wachtOpstartProject)
	{
		return (r, q, cb) -> cb.equal(r.get(Dossier_.wachtOpStartProject), wachtOpstartProject);
	}
}
