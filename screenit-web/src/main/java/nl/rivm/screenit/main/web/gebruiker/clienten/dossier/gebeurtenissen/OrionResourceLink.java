package nl.rivm.screenit.main.web.gebruiker.clienten.dossier.gebeurtenissen;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2021 Facilitaire Samenwerking Bevolkingsonderzoek
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

import nl.topicuszorg.spring.injection.SpringBeanProvider;

import org.apache.wicket.markup.html.link.ResourceLink;
import org.apache.wicket.model.IModel;

import java.io.File;

public class OrionResourceLink extends ResourceLink<Void>
{

	private static final long serialVersionUID = 1L;

	public OrionResourceLink(String id, IModel<File> formulier)
	{
		super(id, new ScannedFormulierViewerResource(formulier, false));
	}

	public OrionResourceLink(String id, String objid)
	{
		super(id, new ScannedFormulierViewerResourceExternal(String.format(SpringBeanProvider.getInstance().getBean(String.class, "antwoordFormulierUrl"), objid), false));
	}
}
