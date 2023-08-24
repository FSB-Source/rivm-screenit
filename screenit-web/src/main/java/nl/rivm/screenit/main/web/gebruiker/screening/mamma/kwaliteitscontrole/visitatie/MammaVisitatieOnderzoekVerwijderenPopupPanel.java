package nl.rivm.screenit.main.web.gebruiker.screening.mamma.kwaliteitscontrole.visitatie;

/*-
 * ========================LICENSE_START=================================
 * screenit-web
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

import nl.rivm.screenit.main.service.mamma.MammaKwaliteitscontroleService;
import nl.rivm.screenit.main.web.component.ScreenitForm;
import nl.rivm.screenit.model.GbaPersoon;
import nl.rivm.screenit.model.mamma.MammaVisitatieOnderzoek;
import nl.rivm.screenit.util.DateUtil;
import nl.topicuszorg.wicket.component.link.IndicatingAjaxSubmitLink;

import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.basic.Label;
import org.apache.wicket.markup.html.form.Form;
import org.apache.wicket.markup.html.panel.GenericPanel;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public abstract class MammaVisitatieOnderzoekVerwijderenPopupPanel extends GenericPanel<MammaVisitatieOnderzoek>
{
	@SpringBean
	private MammaKwaliteitscontroleService kwaliteitscontroleService;

	public MammaVisitatieOnderzoekVerwijderenPopupPanel(String id, IModel<MammaVisitatieOnderzoek> visitatieOnderzoekModel)
	{
		super(id, visitatieOnderzoekModel);
		Form<MammaVisitatieOnderzoek> form = new ScreenitForm<>("form", visitatieOnderzoekModel);
		form.add(new Label("beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.geboortedatum"));
		form.add(new Label("beoordeling.onderzoek.afspraak.uitnodiging.screeningRonde.dossier.client.persoon.bsn"));
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
				MammaVisitatieOnderzoek visitatieOnderzoek = getModelObject();
				GbaPersoon persoon = visitatieOnderzoek.getBeoordeling().getOnderzoek().getAfspraak().getUitnodiging().getScreeningRonde().getDossier().getClient()
					.getPersoon();
				if (visitatieOnderzoek.getId() != null)
				{
					try
					{
						kwaliteitscontroleService.deleteVisitatieOnderzoek(visitatieOnderzoek);
						onOpslaanSuccesvol(target);
					}
					catch (Exception e)
					{
						warn(String.format(getString("error"), persoon.getBsn(), DateUtil.getGeboortedatum(persoon),
								e instanceof IllegalStateException ? e.getMessage() : getString("error.onbekend")));
					}
				}
				else
				{
					warn(String.format(getString("error"), persoon.getBsn(), DateUtil.getGeboortedatum(persoon), getString("error.null")));
				}
			}
		};
	}

	protected abstract void onOpslaanSuccesvol(AjaxRequestTarget target);
}
