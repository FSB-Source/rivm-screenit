package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.fotobespreking;

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

import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.mamma.MammaFotobespreking;
import nl.rivm.screenit.util.NaamUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public abstract class MammaFotobesprekingVerwijderenPopupPanel extends GenericPanel<MammaFotobespreking>
{
	private static final Logger LOG = LoggerFactory.getLogger(MammaFotobesprekingVerwijderenPopupPanel.class);

	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	public MammaFotobesprekingVerwijderenPopupPanel(String id, IModel<MammaFotobespreking> fotobesprekingModel)
	{
		super(id, fotobesprekingModel);
		Form<MammaFotobespreking> form = new ScreenitForm<>("form", fotobesprekingModel);
		form.add(new Label("omschrijving"));
		form.add(new Label("aangemaaktDoor.medewerker.achternaam", NaamUtil.getNaamGebruiker(getModelObject().getAangemaaktDoor().getMedewerker())));
		form.add(createSubmitLink());
		add(form);
	}

	private IndicatingAjaxSubmitLink createSubmitLink()
	{
		return new IndicatingAjaxSubmitLink("submit")
		{
			@Override
			protected void onSubmit(AjaxRequestTarget target)
			{
				MammaFotobespreking fotobespreking = getModelObject();
				try
				{
					kwaliteitscontroleService.deleteFotobespreking(fotobespreking);
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
