package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import lombok.extern.slf4j.Slf4j;

import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.model.mamma.MammaVisitatie;
import nl.rivm.screenit.util.NaamUtil;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.extensions.ajax.markup.html.IndicatingAjaxLink;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

@Slf4j
public abstract class MammaVisitatieVerwijderenPopupPanel extends GenericPanel<MammaVisitatie>
{
	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	protected MammaVisitatieVerwijderenPopupPanel(String id, IModel<MammaVisitatie> visitatieModel)
	{
		super(id, visitatieModel);
		add(new Label("omschrijving"));
		add(new Label("aangemaaktDoor.medewerker.achternaam", NaamUtil.getNaamGebruiker(getModelObject().getAangemaaktDoor().getMedewerker())));
		add(createLink());
	}

	private IndicatingAjaxLink<Void> createLink()
	{
		return new IndicatingAjaxLink<>("verwijder")
		{
			@Override
			public void onClick(AjaxRequestTarget target)
			{
				MammaVisitatie visitatie = MammaVisitatieVerwijderenPopupPanel.this.getModelObject();
				try
				{
					kwaliteitscontroleService.deleteVisitatie(visitatie);
					onOpslaanSuccesvol(target);
				}
				catch (Exception e)
				{
					error(getString("error.onbekend"));
					LOG.error(getString("error.onbekend"), e);
				}
			}
		};
	}

	protected abstract void onOpslaanSuccesvol(AjaxRequestTarget target);
}
