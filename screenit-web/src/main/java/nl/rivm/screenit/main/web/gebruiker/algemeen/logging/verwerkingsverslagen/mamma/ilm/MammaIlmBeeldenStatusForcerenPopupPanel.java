package nl.rivm.screenit.main.web.gebruiker.algemeen.logging.verwerkingsverslagen.mamma.ilm;

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

import nl.rivm.screenit.main.web.ScreenitSession;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.verwerkingverslag.mamma.MammaIlmBeeldenStatusRapportageEntry;
import nl.rivm.screenit.service.mamma.MammaBaseIlmService;
import nl.rivm.screenit.service.mamma.MammaBaseOnderzoekService;
import nl.rivm.screenit.service.mamma.MammaBaseUitwisselportaalService;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaIlmBeeldenStatusForcerenPopupPanel extends GenericPanel<MammaIlmBeeldenStatusRapportageEntry>
{
	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaBaseIlmService baseIlmService;

	@SpringBean
	private MammaBaseUitwisselportaalService baseUitwisselportaalService;

	@SpringBean
	private MammaBaseOnderzoekService baseOnderzoekService;

	public MammaIlmBeeldenStatusForcerenPopupPanel(String id, IModel<MammaIlmBeeldenStatusRapportageEntry> rapportageEntryModel)
	{
		super(id, rapportageEntryModel);
		Form<MammaIlmBeeldenStatusRapportageEntry> form = new ScreenitForm<>("form", rapportageEntryModel);
		form.add(new Label("accessionNumber"));
		form.add(new Label("client.persoon.geboortedatum"));
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
				MammaIlmBeeldenStatusRapportageEntry entry = getModelObject();
				boolean geforceerd = baseIlmService.forceerIlmStatusVerwijderd(entry, ScreenitSession.get().getLoggedInInstellingGebruiker());
				if (geforceerd)
				{
					info(getString("geforceerd.true"));
				}
				else
				{
					warn(getString("geforceerd.false"));
				}
				onOpslaanSuccesvol(target);
			}
		};
	}

	protected abstract void onOpslaanSuccesvol(AjaxRequestTarget target);
}
