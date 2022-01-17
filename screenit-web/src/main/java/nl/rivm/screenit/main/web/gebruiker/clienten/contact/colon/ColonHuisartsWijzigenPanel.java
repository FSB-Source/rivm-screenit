package nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon;

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

import java.util.List;
import java.util.Map;

import nl.rivm.screenit.model.enums.ExtraOpslaanKey;
import nl.rivm.screenit.main.web.component.modal.BootstrapDialog;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.AbstractClientContactActiePanel;
import nl.rivm.screenit.main.web.gebruiker.clienten.contact.colon.huisarts.HuisartsPanel;
import nl.rivm.screenit.model.Client;
import nl.rivm.screenit.model.ClientContactActie;
import nl.rivm.screenit.model.colon.ColonScreeningRonde;
import nl.rivm.screenit.model.enums.Bevolkingsonderzoek;
import nl.rivm.screenit.model.enums.BezwaarType;
import nl.rivm.screenit.util.BezwaarUtil;
import nl.rivm.screenit.util.EntityAuditUtil;
import nl.topicuszorg.hibernate.spring.dao.HibernateService;
import nl.topicuszorg.wicket.hibernate.util.ModelUtil;

import org.apache.commons.lang3.StringUtils;
import org.apache.wicket.ajax.AjaxRequestTarget;
import org.apache.wicket.markup.html.WebMarkupContainer;
import org.apache.wicket.model.IModel;
import org.apache.wicket.model.Model;
import org.apache.wicket.spring.injection.annot.SpringBean;

public class ColonHuisartsWijzigenPanel extends AbstractClientContactActiePanel<ClientContactActie>
{

	private static final long serialVersionUID = 1L;

	protected IModel<ColonScreeningRonde> colonScreeningRonde;

	private IModel<Boolean> huisartsBerichtenVerzenden = Model.of(Boolean.FALSE);

	private BootstrapDialog dialog;

	private WebMarkupContainer huisartsContainer;

	@SpringBean
	private HibernateService hibernateService;

	public ColonHuisartsWijzigenPanel(String id, IModel<ClientContactActie> model, IModel<Client> clientModel, List<Object> extraPanelParams)
	{
		super(id, model);
		colonScreeningRonde = ModelUtil.cModel(clientModel.getObject().getColonDossier().getLaatsteScreeningRonde());

		dialog = (BootstrapDialog) extraPanelParams.stream().filter(p -> p instanceof BootstrapDialog).findFirst().orElse(null);

		huisartsContainer = new WebMarkupContainer("huisartsContainer");
		huisartsContainer.setOutputMarkupId(true);
		huisartsContainer.addOrReplace(maakHuisartsPanel());
		add(huisartsContainer);
	}

	private WebMarkupContainer maakHuisartsPanel()
	{
		HuisartsPanel huisartsPanel = new HuisartsPanel("huisartsPanel", colonScreeningRonde, this, dialog, huisartsBerichtenVerzenden);
		huisartsPanel.setOutputMarkupId(true);
		return huisartsPanel;
	}

	public void verversHuisarts(AjaxRequestTarget target)
	{
		huisartsContainer.addOrReplace(maakHuisartsPanel());
		target.add(huisartsContainer);
	}

	@Override
	public Map<ExtraOpslaanKey, Object> getOpslaanObjecten()
	{
		Map<ExtraOpslaanKey, Object> opslaanObjecten = super.getOpslaanObjecten();
		opslaanObjecten.put(ExtraOpslaanKey.COLON_HUISARTS, colonScreeningRonde.getObject());
		opslaanObjecten.put(ExtraOpslaanKey.COLON_HUISARTSBERICHTEN_VERZENDEN, huisartsBerichtenVerzenden.getObject());
		return opslaanObjecten;
	}

	@Override
	public List<String> getOpslaanMeldingen()
	{
		List<String> opslaanMeldingen = super.getOpslaanMeldingen();
		ColonScreeningRonde screeningRonde = colonScreeningRonde.getObject();
		if (Boolean.TRUE.equals(huisartsBerichtenVerzenden.getObject())
			&& BezwaarUtil.isBezwaarActiefVoor(screeningRonde.getDossier().getClient(), BezwaarType.GEEN_UITWISSELING_MET_DE_HUISARTS, Bevolkingsonderzoek.COLON)
			&& screeningRonde.getColonHuisarts() != null)
		{
			opslaanMeldingen.add("Er is nog een bezwaar voor uitwisseling met de huisarts voor deze client actief. Huisartsbericht wordt niet verstuurd.");
		}
		return opslaanMeldingen;
	}

	@Override
	public void validate()
	{
		super.validate();
		ColonScreeningRonde screeningRonde = colonScreeningRonde.getObject();
		String diffColonHuisarts = EntityAuditUtil.getDiffFieldToLatestVersion(screeningRonde, "colonHuisarts", hibernateService.getHibernateSession());
		String diffOnbekendeHuisarts = EntityAuditUtil.getDiffFieldToLatestVersion(screeningRonde, "onbekendeHuisarts", hibernateService.getHibernateSession());

		if (StringUtils.isBlank(diffColonHuisarts) && StringUtils.isBlank(diffOnbekendeHuisarts))
		{
			error("Er is geen wijziging doorgevoerd in de huisarts darmkanker vastleggen contact actie.");
		}

	}

	@Override
	protected void onDetach()
	{
		super.onDetach();
		ModelUtil.nullSafeDetach(colonScreeningRonde);
	}
}
