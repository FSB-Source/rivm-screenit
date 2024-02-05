package nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma;

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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.mamma.huisarts.MammaHuisartsBeheerPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.EnovationHuisarts;
import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.model.mamma.MammaScreeningRonde;
import nl.rivm.screenit.model.mamma.enums.MammaGeenHuisartsOption;
import nl.rivm.screenit.service.mamma.MammaHuisartsService;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class MammaHuisartsWijzigenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	@SpringBean
	private HibernateService hibernateService;

	@SpringBean
	private MammaHuisartsService huisartsService;

	private final IModel<MammaScreeningRonde> screeningRondeModel;

	private final BootstrapDialog dialog;

	private final WebMarkupContainer huisartsContainer;

	public MammaHuisartsWijzigenPanel(String id, IModel<ClientContactActie> model, IModel<Client> clientModel, List<Object> extraPanelParams)
	{
		super(id, model);
		screeningRondeModel = ModelUtil.ccModel(clientModel.getObject().getMammaDossier().getLaatsteScreeningRonde());

		dialog = (BootstrapDialog) extraPanelParams.stream().filter(BootstrapDialog.class::isInstance).findFirst().orElse(null);

		huisartsContainer = new WebMarkupContainer("huisartsContainer");
		huisartsContainer.setOutputMarkupId(true);
		huisartsContainer.addOrReplace(maakHuisartsPanel());
		add(huisartsContainer);
	}

	private WebMarkupContainer maakHuisartsPanel()
	{
		MammaHuisartsBeheerPanel huisartsPanel = new MammaHuisartsBeheerPanel("huisartsPanel", screeningRondeModel, dialog)
		{
			@Override
			protected void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie)
			{
				MammaHuisartsWijzigenPanel.this.onHuisartsGekozen(target, huisarts, geenHuisartsOptie);
			}

			@Override
			protected EnovationHuisarts getHuisartsVorigeRonde()
			{
				return huisartsService.getActieveHuisartsVanVorigeRonde(getModelObject());
			}

		};
		huisartsPanel.setOutputMarkupId(true);
		return huisartsPanel;
	}

	private void onHuisartsGekozen(AjaxRequestTarget target, EnovationHuisarts huisarts, MammaGeenHuisartsOption geenHuisartsOptie)
	{
		MammaScreeningRonde screeningRonde = screeningRondeModel.getObject();
		screeningRonde.setHuisarts(huisarts);
		screeningRonde.setGeenHuisartsOptie(geenHuisartsOptie);
		huisartsContainer.addOrReplace(maakHuisartsPanel());
		target.add(huisartsContainer);
		dialog.close(target);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		opslaanObjecten.put(ExtraOpslaanKey.MAMMA_HUISARTS, screeningRondeModel.getObject());
		return opslaanObjecten;
	}

	@Override
	public void validate()
	{
		super.validate();
		MammaScreeningRonde screeningRonde = screeningRondeModel.getObject();
		var isHuisartsGewijzigd = StringUtils.isNotBlank(
			EntityAuditUtil.getDiffFieldsToLatestVersion(screeningRonde, hibernateService.getHibernateSession(), "geenHuisartsOptie", "huisarts"));

		if (!isHuisartsGewijzigd)
		{
			error(getString("huisarts.niet.gewijzigd"));
		}
	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(screeningRondeModel);
	}
}
