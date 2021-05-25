package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding;

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

import nl.rivm.screenit.model.mamma.enums.MammaAfbeeldingZijdeDoorsnede;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.markup.html.panel.EmptyPanel;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.markup.html.panel.Panel;
import org.apache.wicket.model.IModel;

public abstract class MammaBaseAfbeeldingPanel<T> extends GenericPanel<T>
{
	private static final long serialVersionUID = 1L;

	public MammaBaseAfbeeldingPanel(String id, IModel<T> model)
	{
		super(id, model);
	}

	@Override
	protected void onInitialize()
	{
		super.onInitialize();
		addContainerWithImage(this, MammaAfbeeldingZijdeDoorsnede.RECHTS_VERTICALE_DOORSNEDE);
		addContainerWithImage(this, MammaAfbeeldingZijdeDoorsnede.LINKS_VERTICALE_DOORSNEDE);
		addContainerWithImage(this, MammaAfbeeldingZijdeDoorsnede.RECHTS_HORIZONTALE_DOORSNEDE);
		addContainerWithImage(this, MammaAfbeeldingZijdeDoorsnede.LINKS_HORIZONTALE_DOORSNEDE);
		add(createBeoordelingIconenPanel("beoordelingIconenPanel"));
	}

	private void addContainerWithImage(WebMarkupContainer imagesContainer, MammaAfbeeldingZijdeDoorsnede zijdeDoorsnede)
	{
		WebMarkupContainer imageContainer = new WebMarkupContainer(zijdeDoorsnede.getSvgFileName() + "Container");
		imagesContainer.add(imageContainer);
		addImage(imageContainer, zijdeDoorsnede);
	}

	protected abstract void addImage(WebMarkupContainer imageContainer, MammaAfbeeldingZijdeDoorsnede zijdeDoorsnede);

	protected Panel createBeoordelingIconenPanel(String id)
	{
		return new EmptyPanel(id);
	}
}
