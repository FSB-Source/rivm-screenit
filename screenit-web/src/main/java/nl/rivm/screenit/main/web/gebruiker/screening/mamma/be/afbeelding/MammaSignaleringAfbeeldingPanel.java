package nl.rivm.screenit.main.web.gebruiker.screening.mamma.be.afbeelding;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
 * %%
 * Copyright (C) 2012 - 2022 Facilitaire Samenwerking Bevolkingsonderzoek
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

import java.io.InputStream;

import nl.rivm.screenit.main.web.component.SvgImage;
import nl.rivm.screenit.model.mamma.MammaAnnotatieAfbeelding;
import nl.rivm.screenit.model.mamma.MammaSignaleren;
import nl.rivm.screenit.model.mamma.enums.MammaAfbeeldingZijdeDoorsnede;
import nl.rivm.screenit.service.mamma.MammaBaseAfbeeldingService;

import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.PropertyModel;
import org.apache.wicket.request.resource.IResource.Attributes;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaSignaleringAfbeeldingPanel extends MammaBaseAfbeeldingPanel<MammaSignaleren>
{
	private static final long serialVersionUID = 1L;

	@SpringBean
	private MammaBaseAfbeeldingService afbeeldingService;

	public MammaSignaleringAfbeeldingPanel(String id, IModel<MammaSignaleren> model)
	{
		super(id, model);
	}

	@Override
	protected void addImage(WebMarkupContainer imageContainer, MammaAfbeeldingZijdeDoorsnede zijdeDoorsnede)
	{
		imageContainer.add(new SvgImage(zijdeDoorsnede.getSvgFileName())
		{
			@Override
			protected InputStream getSvgImageData(Attributes attributes)
			{
				MammaAnnotatieAfbeelding afbeelding = new PropertyModel<MammaAnnotatieAfbeelding>(
					MammaSignaleringAfbeeldingPanel.this.getModel(), zijdeDoorsnede.getSignalerenDoorsnedeProperty()).getObject();
				return afbeeldingService.createSignaleringAfbeelding(afbeelding, zijdeDoorsnede,
					MammaSignaleringAfbeeldingPanel.this.getModelObject().getOnderzoek().getAmputatie());
			}
		});
	}
}
